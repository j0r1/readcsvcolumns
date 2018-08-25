#ifdef _WIN32
#include <windows.h>
#endif // _WIN32

#include <Rcpp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <string>
#include <iostream>

#ifdef _WIN32
#define STRTOK_R strtok_s
#else
#define STRTOK_R strtok_r
#endif // _WIN32

using namespace std;
using namespace Rcpp;

// The following two functions are slow, but are only used for the 
// first line containing labels
bool ReadInputLine(FILE *fi, string &line);
void SplitLine(const string &line, vector<string> &args, const string &separatorChars,
	       const string &quoteChars, const string &commentStartChars, bool ignoreZeroLengthFields);

class ValueVector
{
public:
	enum VectorType { Ignore, Integer, Double, String };

	ValueVector(VectorType t = Ignore);
	~ValueVector();

	void setType(VectorType t);
	bool ignore() const 						{ return m_vectorType == Ignore; }

	bool processWithCheck(const char *pStr, bool lastCol);

	void setName(const std::string &n) 				{ m_name = n; }
	const string getName() const 					{ return m_name; }

	void addColumnToList(List &listOfVectors);
private:
	static const char *skipWhite(const char *pStr);
	static bool parseAsInt(const char *pStr, int &value);
	static bool parseAsDouble(const char *pStr, double &value);
	
	VectorType m_vectorType;
	string m_name;

	vector<int> m_vectorInt;
	vector<double> m_vectorDouble;
	vector<string> m_vectorString;
};

class AutoCloseFile
{
public:
	AutoCloseFile(FILE *pFile) : m_pFile(pFile) 			{ }
	~AutoCloseFile() 						{ if (m_pFile) fclose(m_pFile); }
private:
	FILE *m_pFile;
};

void Throw(const char *format, ...)
{
#define MAXLEN 16384

        va_list ap;
	char buf[MAXLEN];

        va_start(ap, format);
        vsnprintf(buf, MAXLEN, format, ap);
        va_end(ap);

	buf[MAXLEN-1] = 0;
	throw Rcpp::exception(buf);
}

string GetColumnSpecAndColumnNames(string fileName, FILE *pFile, string columnSpec, bool hasHeaders, vector<string> &names)
{
	names.clear();
	string line;

	if (!ReadInputLine(pFile, line))
		Throw("Unable to read first line from file '%s'", fileName.c_str());

	vector<string> parts;
	SplitLine(line, parts, ",", "\"'", "", false);

	const size_t numCols = parts.size();

	if (columnSpec.length() > 0)
	{
		if (columnSpec.length() != numCols)
			Throw("Number of columns in first line (%u) is not equal to the column specification length (%u)", parts.size(), columnSpec.length());

		if (!hasHeaders) // Need to rewind the file
		{
			if (fseek(pFile, 0, SEEK_SET) != 0)
				Throw("Unable to rewind the file (needed after checking number of columns)");
		}
	}
	else // try to guess the types
	{
		vector<string> guessParts = parts;		

		if (hasHeaders) // In this case, we need the second line
		{
			if (!ReadInputLine(pFile, line))
				Throw("Unable to read second line from file '%s' (needed to guess column types)", fileName.c_str());

			SplitLine(line, guessParts, ",", "", "", false);
			
			if (guessParts.size() != parts.size())
				Throw("First and second line in '%s' do not contain the same number of columns (%u vs %u)", fileName.c_str(), parts.size(), guessParts.size());
		}

		for (int i = 0 ; i < numCols ; i++)
		{
			ValueVector testVec;

			testVec.setType(ValueVector::Integer);
			if (testVec.processWithCheck(guessParts[i].c_str(), false))
			{
				columnSpec += "i";
				continue;
			}

			testVec.setType(ValueVector::Double);
			if (testVec.processWithCheck(guessParts[i].c_str(), false))
			{
				columnSpec += "r";
				continue;
			}

			// If neither integer nor double works, lets use a string
			columnSpec += "s";
		}

		Rcout << "Detected column specification is '" << columnSpec << "'" << endl;

		if (fseek(pFile, 0, SEEK_SET) != 0)
			Throw("Unable to rewind the file (needed after establising the column types)");

		if (hasHeaders)
		{
			// In this case, we need to skip the first line again
			if (!ReadInputLine(pFile, line))
				Throw("Unable to re-read the first line (needed after establising the column types)");
		}
	}

	if (hasHeaders)
		names = parts;

	return columnSpec;
}

// [[Rcpp::export]]
List ReadCSVColumns(string fileName, string columnSpec, int maxLineLength, bool hasHeaders) 
{
	if (maxLineLength <= 0)
		Throw("Maximum line length must be larger than 0 (is %d)", maxLineLength);

	FILE *pFile = fopen(fileName.c_str(), "rt");
	if (!pFile)
		Throw("Unable to open file '%s'", fileName.c_str());

	AutoCloseFile autoCloser(pFile);
	vector<string> names;

	columnSpec = GetColumnSpecAndColumnNames(fileName, pFile, columnSpec, hasHeaders, names);

	const size_t numCols = columnSpec.length();
	vector<ValueVector> columns(numCols);

	for (size_t i = 0 ; i < numCols ; i++)
	{
		switch(columnSpec[i])
		{
		case 'i':
			columns[i].setType(ValueVector::Integer);
			break;
		case 'r':
			columns[i].setType(ValueVector::Double);
			break;
		case 's':
			columns[i].setType(ValueVector::String);
			break;
		case '.':
			columns[i].setType(ValueVector::Ignore);
			break;
		default:
			Throw("Invalid column type '%c'", columnSpec[i]);
		}
	}

	if (hasHeaders)
	{
		for (size_t i = 0 ; i < numCols ; i++)
			columns[i].setName(names[i]);
	}

	vector<char> buffer(maxLineLength);
	char *buff = &(buffer[0]);
	int lineNumber = 2;
	int numElements = 0;

	while (fgets(buff, maxLineLength, pFile))
	{
		buff[maxLineLength-1] = 0;

		char *pBuff = buff;
		char *pPtr = 0;

		for (int i = 0 ; i < numCols ; i++)
		{
			char *pPart = STRTOK_R(pBuff, ",", &pPtr);
			pBuff = 0;

			if (!pPart)
				Throw("Not enough columns on line %d", lineNumber);

			int colNum = i+1;
			if (!columns[i].processWithCheck(pPart, colNum == numCols))
			{
				Throw("Unable to interpret '%s' (line %d, col %d) as type '%c'",
				      pPart, lineNumber, colNum, columnSpec[i]);
			}
		}

		lineNumber++;
		numElements++;
	}

	//cout << "Data loaded, storing in R struct" << endl;

	List listOfVectors;
	CharacterVector nameVec;

	size_t listPos = 0;
	for (size_t i = 0 ; i < columns.size() ; i++)
	{
		if (!columns[i].ignore())
		{
			if (hasHeaders)
				nameVec.push_back(columns[i].getName());

			columns[i].addColumnToList(listOfVectors);
			listPos++;
		}
	}

	if (hasHeaders)
		listOfVectors.attr("names") = nameVec;

	return listOfVectors;
}

//////////////////////////////////////////////////////////////////////////////

bool ReadInputLine(FILE *fi, string &line)
{
	if (fi == 0)
		return false;

	vector<char> data;
	bool gotchar = false;
	int c;

	while ((c = fgetc(fi)) != EOF)
	{
		gotchar = true;
		if (c == '\n') // stop here
			break;

		data.push_back((char)c);
	}

	if (!gotchar)
		return false;

	size_t l = data.size();
	if (l == 0)
		line = "";
	else
	{
		// Make sure it's null-terminated
		if (data[l-1] == '\r')
			data[l-1] = 0;
		else
			data.push_back(0);

		line = string(&(data[0]));
	}

	return true;
}

bool HasCharacter(const string &charList, char c)
{
	for (int i = 0 ; i < charList.length() ; i++)
	{
		if (c == charList[i])
			return true;
	}
	return false;
}

void SplitLine(const string &line, vector<string> &args, const string &separatorChars,
	       const string &quoteChars, const string &commentStartChars, bool ignoreZeroLengthFields)
{
	vector<string> arguments;
	int startPos = 0;

	while (startPos < line.length() && HasCharacter(separatorChars, line[startPos]))
	{
		startPos++;

		if (!ignoreZeroLengthFields)
			arguments.push_back("");
	}

	string curString("");
	bool done = false;

	if (startPos >= line.length())
	{
		if (!ignoreZeroLengthFields)
			arguments.push_back("");
	}

	while (startPos < line.length() && !done)
	{
		int endPos = startPos;
		bool endFound = false;
		bool gotSeparator = false;

		while (!endFound && endPos < line.length())
		{
			if (HasCharacter(separatorChars, line[endPos]) || HasCharacter(commentStartChars, line[endPos]))
			{
				curString += line.substr(startPos, endPos-startPos);
				endFound = true;

				if (HasCharacter(separatorChars, line[endPos]))
				{
					gotSeparator = true;
					endPos++;
				}
			}
			else if (HasCharacter(quoteChars, line[endPos]))
			{
				curString += line.substr(startPos, endPos-startPos);

				char quoteStartChar = line[endPos];

				endPos += 1;
				startPos = endPos;

				while (endPos < line.length() && line[endPos] != quoteStartChar)
					endPos++;

				curString += line.substr(startPos, endPos-startPos);

				if (endPos < line.length())
					endPos++;

				startPos = endPos;
			}
			else
				endPos++;
		}

		if (!endFound)
		{
			if (endPos-startPos > 0)
				curString += line.substr(startPos, endPos-startPos);
		}

		if (curString.length() > 0 || !ignoreZeroLengthFields)
			arguments.push_back(curString);

		if (endPos < line.length() && HasCharacter(commentStartChars, line[endPos]))
			done = true;
		else
		{
			startPos = endPos;
			curString = string("");


			while (startPos < line.length() && HasCharacter(separatorChars, line[startPos]))
			{
				gotSeparator = true;
				startPos++;

				if (!ignoreZeroLengthFields)
					arguments.push_back("");
			}
			
			if (gotSeparator)
			{
				if (startPos >= line.length())
				{
					if (!ignoreZeroLengthFields)
						arguments.push_back("");
				}
			}
		}
	}

	args = arguments;
}

ValueVector::ValueVector(VectorType t) : m_vectorType(t) 
{ 
}

ValueVector::~ValueVector() 
{ 
}

void ValueVector::setType(VectorType t)
{
	if (m_vectorInt.size() || m_vectorDouble.size() || m_vectorString.size())
		throw Rcpp::exception("Internal error: vectors should be empty when calling setType()");

	m_vectorType = t;
}

bool ValueVector::processWithCheck(const char *pStr, bool lastCol)
{ 
	switch(m_vectorType)
	{
	case Ignore:
		break;
	case Integer:
		int x;
		if (!parseAsInt(pStr, x))
			return false;

		m_vectorInt.push_back(x);
		break;
	case Double:
		double y;
		if (!parseAsDouble(pStr, y))
			return false;

		m_vectorDouble.push_back(y);
		break;
	case String:
		if (lastCol) // Possibly ends with \n, \r\n
		{
			int len = strlen(pStr);

			while (len > 0)
			{
				char c = pStr[len-1];
				if (c == '\n' || c == '\r')
				{
					len--;
				}
				else
				{
					break;
				}
			}
			m_vectorString.push_back(string(pStr, len));
		}
		else
		{
			m_vectorString.push_back(pStr);
		}
		break;
	default:
		throw Rcpp::exception("Internal error: unknown m_vectorType");
	}

	return true;
}

void ValueVector::addColumnToList(List &listOfVectors)
{
	switch(m_vectorType)
	{
	case Ignore:
		throw Rcpp::exception("Internal error: 'Ignore' should not be used in addColumnToList");
	case Integer:
		{
			const int num = m_vectorInt.size();
			IntegerVector v(num);

			for (int i = 0 ; i < num ; i++)
				v[i] = m_vectorInt[i];

			listOfVectors.push_back(v);
		}
		break;
	case Double:
		{
			const int num = m_vectorDouble.size();
			NumericVector v(num);

			for (int i = 0 ; i < num ; i++)
				v[i] = m_vectorDouble[i];

			listOfVectors.push_back(v);
		}
		break;
	case String:
		{
			const int num = m_vectorString.size();
			StringVector v(num);

			for (int i = 0 ; i < num ; i++)
				v[i] = m_vectorString[i];

			listOfVectors.push_back(v);
		}
		break;
	default:
		throw Rcpp::exception("Internal error: unknown m_vectorType");
	}

}

inline const char *ValueVector::skipWhite(const char *pStr)
{
	while (1)
	{
		char c = *pStr;
		if (c == '\0' || !(c == ' ' || c == '\t' || c == '\r' || c == '\n'))
			break;
		pStr++;
	}
	return pStr;
}

bool ValueVector::parseAsInt(const char *pStr, int &value)
{
	pStr = skipWhite(pStr);
	if (*pStr == '\0')
		return false;

	char *endptr;
	long int v = strtol(pStr,&endptr,10); // base 10
	const char *endptr2 = skipWhite(endptr);

	if (*endptr2 != '\0')
		return false;

	value = (int)v;

	if ((long)value != v)
		return false;

	return true;
}

bool ValueVector::parseAsDouble(const char *pStr, double &value)
{
	pStr = skipWhite(pStr);
	if (*pStr == '\0')
		return false;

	char *endptr;
	
	value = strtod(pStr, &endptr);
	const char *endptr2 = skipWhite(endptr);

	if (*endptr2 != '\0')
	{
		// Try to compensate for things like 1.#INF on windows
		// Will not strictly be correct since we've already skipped
		// whitespace, so '1.    #INF' will also be detected as
		// infinity
		if (endptr2[0] == '#' && endptr2[1] == 'I' && endptr2[2] == 'N')
		{
			if (endptr2[3] == 'F') // #INF
			{
				// assume its +/- inf, without further checking
				if (value < 0)
				{
					value = -std::numeric_limits<double>::infinity();
					return true;
				}
				value = std::numeric_limits<double>::infinity();
				return true;
			}
			if (endptr2[3] == 'D') // #IND
			{
				// Assume it's NaN
				value = std::numeric_limits<double>::quiet_NaN();
				return true;
			}
		}
		return false;
	}
	return true;
}


