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

#ifndef _WIN32
#include <sys/mman.h>
#include <unistd.h>
#include "jthread.h"

using namespace jthread;
#endif // _WIN32

using namespace std;
using namespace Rcpp;

// The following two functions are slow, but are only used for the 
// first line containing labels
bool ReadInputLine(FILE *fi, string &line);
void SplitLine(const string &line, vector<string> &args, const string &separatorChars,
	       const string &quoteChars, const string &commentStartChars, bool ignoreZeroLengthFields);
char *StrTok(char *pStr, char delim, char **pSavePtr);

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
	void addColumnToList(List &listOfVectors, int numThreads, int thread, int totalEntries);
	int getEntries() const;
private:
	static char *skipWhite(char *pStr);
	static const char *skipWhite(const char *pStr);
	static bool parseAsInt(const char *pStr, int &value);
	static bool parseAsDouble(const char *pStr, double &value);
	
	VectorType m_vectorType;
	string m_name;

	vector<int> m_vectorInt;
	vector<double> m_vectorDouble;
	vector<string> m_vectorString;
};

#ifndef _WIN32
class ParserThread : public JThread
{
public:
	ParserThread(vector<ValueVector> &cols, string &errStr, int tNum, int threads,
	             char *pStartStr, int nCols, bool hasHeaders, int maxLen,
		     const string &colSpec, volatile bool &intr) 
		: columns(cols), errorString(errStr), threadNum(tNum), numThreads(threads), 
		  numCols(nCols), maxLineLength(maxLen), pStr(pStartStr), columnSpec(colSpec),
		  interrupt(intr)
	{
		lineNumber = (hasHeaders)?2:1;
		m_endMutex.Init();
	}

	~ParserThread() { }

	void runThread();
	void *Thread();

	JMutex m_endMutex;
private:
	vector<ValueVector> &columns;
	string &errorString;
	const int threadNum, numThreads, numCols, maxLineLength;
	char *pStr;
	int lineNumber;
	string columnSpec;
	volatile bool &interrupt;
};
#endif // !_WIN32

inline char *ValueVector::skipWhite(char *pStr)
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

inline bool ValueVector::parseAsInt(const char *pStr, int &value)
{
	pStr = skipWhite(pStr);
	if (*pStr == '\0')
		return false;

	char *endptr;
	long int v = strtol(pStr,&endptr,10); // base 10
	endptr = skipWhite(endptr);

	if (*endptr != '\0')
	{
		if (endptr[0] == 'N' && endptr[1] == 'A')
		{
			char c = endptr[2];

			if (c == '\0' || c == ' ' || c == '\t' || c == '\r' || c == '\n') // Assume it's just NA
			{
				value = NA_INTEGER;
				return true;
			}
		}
		
		return false;
	}

	value = (int)v;
	if ((long)value != v) // Doesn't fit in an 'int'
		return false;

	if (value == NA_INTEGER) // Reserved!
		return false;

	return true;
}

inline bool ValueVector::parseAsDouble(const char *pStr, double &value)
{
	pStr = skipWhite(pStr);
	if (*pStr == '\0')
		return false;

	char *endptr;
	
	value = strtod(pStr, &endptr);
	endptr = skipWhite(endptr);

	if (*endptr != '\0')
	{
		// Try to compensate for things like 1.#INF on windows
		// Will not strictly be correct since we've already skipped
		// whitespace, so '1.    #INF' will also be detected as
		// infinity
		if (endptr[0] == '#')
		{
			if (endptr[1] == 'I' && endptr[2] == 'N')
			{
				if (endptr[3] == 'F') // #INF
				{
					// assume its +/- inf, without further checking
					value = (value < 0)?(-std::numeric_limits<double>::infinity()):(std::numeric_limits<double>::infinity());
					return true;
				}
				if (endptr[3] == 'D') // #IND
				{
					// Assume it's NaN
					value = (value < 0)?(-std::numeric_limits<double>::quiet_NaN()):(std::numeric_limits<double>::quiet_NaN());
					return true;
				}
			}

			if (endptr[1] == 'Q' && endptr[2] == 'N' && endptr[3] == 'A' && endptr[4] == 'N') // #QNAN
			{
				// Assume it's NaN
				value = (value < 0)?(-std::numeric_limits<double>::quiet_NaN()):(std::numeric_limits<double>::quiet_NaN());
				return true;
			}
			if (endptr[1] == 'S' && endptr[2] == 'N' && endptr[3] == 'A' && endptr[4] == 'N') // #SNAN
			{
				// Assume it's NaN
				value = (value < 0)?(-std::numeric_limits<double>::quiet_NaN()):(std::numeric_limits<double>::quiet_NaN());
				return true;
			}
		}

		if (endptr[0] == 'N' && endptr[1] == 'A')
		{
			char c = endptr[2];

			if (c == '\0' || c == ' ' || c == '\t' || c == '\r' || c == '\n') // Assume it's just NA
			{
				value = NA_REAL;
				return true;
			}
		}

		return false;
	}
	
	return true;
}

inline int ValueVector::getEntries() const
{
	switch(m_vectorType)
	{
	case Ignore:
		return 0;
	case Integer:
		return m_vectorInt.size();
	case Double:
		return m_vectorDouble.size();
	case String:
		return m_vectorString.size();
	default:
		throw Rcpp::exception("Internal error: unknown m_vectorType in getEntries");
	}
}

class AutoCloseFile
{
public:
	AutoCloseFile(FILE *pFile) : m_pFile(pFile) 					{ }
	~AutoCloseFile() 								{ if (m_pFile) fclose(m_pFile); }
private:
	FILE *m_pFile;
};

#ifndef _WIN32
class AutoUnMap
{
public:
	AutoUnMap(void *pMmapAddr, size_t len) : m_pMmapAddr(pMmapAddr), m_len(len)	{ }
	~AutoUnMap()									{ munmap(m_pMmapAddr, m_len); }
private:
	void *m_pMmapAddr;
	size_t m_len;
};
#endif // !_WIN32

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

string getString(const char *format, ...)
{
        va_list ap;
	char buf[MAXLEN];

        va_start(ap, format);
        vsnprintf(buf, MAXLEN, format, ap);
        va_end(ap);

	buf[MAXLEN-1] = 0;
	return string(buf);
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
	else // Use default column names
	{
		char tmp[1024];
		
		names.clear();
		for (int i = 1 ; i <= (int)parts.size() ; i++)
		{
			sprintf(tmp, "col_%03d", i);
			names.push_back(tmp);
		}
	}

	return columnSpec;
}

inline char *gotoNextLine(char *pStr)
{
	char c = '\0';
	while (1)
	{
		c = *pStr;
		if (c == '\0')
			break;

		if (c == '\n')
		{
			pStr++;
			break;
		}

		pStr++;
	}

	return pStr;
}

// [[Rcpp::export]]
List ReadCSVColumns(string fileName, string columnSpec, int maxLineLength, bool hasHeaders, int numThreads) 
{
	if (numThreads < 1)
		Throw("Number of threads must be at least one");

	if (maxLineLength <= 0)
		Throw("Maximum line length must be larger than 0 (is %d)", maxLineLength);

	FILE *pFile = fopen(fileName.c_str(), "rt");
	if (!pFile)
		Throw("Unable to open file '%s'", fileName.c_str());

	AutoCloseFile autoCloser(pFile);
	vector<string> names;

	columnSpec = GetColumnSpecAndColumnNames(fileName, pFile, columnSpec, hasHeaders, names);
	const size_t numCols = columnSpec.length();

	if (numCols == 0)
		Throw("No columns found in file '%s'", fileName.c_str());

	size_t ignoreColumns = 0;
	for (size_t i = 0 ; i < numCols ; i++)
	{
		if (columnSpec[i] == '.')
			ignoreColumns++;
	}

	if (ignoreColumns == numCols)
		Throw("All columns will be ignored by the given column specification");

	List listOfVectors;

#ifdef _WIN32
	if (numThreads != 1)
	{
		numThreads = 1;
		Rcerr << "Parellel interpretation of numbers is not available on Win32 platform, reverting to single thread" << endl;
	}
#endif // _WIN32

	if (numThreads == 1)
	{
		vector<ValueVector> columns(numCols);
		vector<char> buffer(maxLineLength);
		char *buff = &(buffer[0]);

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

		int lineNumber = 2;
		int numElements = 0;

		while (fgets(buff, maxLineLength, pFile))
		{
			buff[maxLineLength-1] = 0;

			char *pBuff = buff;
			char *pPtr = 0;

			for (int i = 0 ; i < numCols ; i++)
			{
				char *pPart = StrTok(pBuff, ',', &pPtr);
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

		CharacterVector nameVec;
		for (size_t i = 0 ; i < columns.size() ; i++)
		{
			if (!columns[i].ignore())
			{
				//if (hasHeaders)
				nameVec.push_back(names[i]);

				columns[i].addColumnToList(listOfVectors);
			}
		}

		//if (hasHeaders)
		listOfVectors.attr("names") = nameVec;
	}
	else // Parallel version using mmap and openmp
	{
#ifndef _WIN32
		int fileDesc = fileno(pFile);
		if (fileDesc < 0)
			Throw("Internal error: unable to get file descriptor of opened file");

		if (fseek(pFile, 0, SEEK_END) != 0)
			Throw("Couldn't seek to the end of the file");

		long fileSize = ftell(pFile);
		
		// Using fileSize+1 to make sure it's zero-terminated
		// From Linux manual page:
		//   For a file that is not a multiple of the page  size,  the  remaining  memory  
		//   is  zeroed
		// From OS X manual page:
		//   'Any extension beyond the end of the mapped object will be zero-filled.
		void *pMmapAddr = mmap(0, fileSize+1, PROT_READ, MAP_PRIVATE, fileDesc, 0);
		if (pMmapAddr == 0)
			Throw("Unable to use 'mmap' to access file");

		AutoUnMap autoUnMap(pMmapAddr, fileSize); // Make sure munmap is called when done

		Rcout << "Using " << numThreads << " threads to parse fields" << endl;

		char *pStrStart = (char *)pMmapAddr;
		if (hasHeaders)
			pStrStart = gotoNextLine(pStrStart);

		vector<vector<ValueVector> > threadColumns(numThreads);
		vector<string> errorReasons(numThreads);

		size_t lastRealColumn = 0;

		for (size_t t = 0 ; t < numThreads ; t++)
		{
			threadColumns[t].resize(numCols);
			for (size_t i = 0 ; i < numCols ; i++)
			{
				switch(columnSpec[i])
				{
				case 'i':
					threadColumns[t][i].setType(ValueVector::Integer);
					lastRealColumn = i;
					break;
				case 'r':
					threadColumns[t][i].setType(ValueVector::Double);
					lastRealColumn = i;
					break;
				case 's':
					threadColumns[t][i].setType(ValueVector::String);
					lastRealColumn = i;
					break;
				case '.':
					threadColumns[t][i].setType(ValueVector::Ignore);
					break;
				default:
					Throw("Invalid column type '%c'", columnSpec[i]);
				}
			}
		}

		volatile bool interrupt = false;

		vector<ParserThread *> parserThreads(numThreads);
		for (int i = 0 ; i < numThreads ; i++)
			parserThreads[i] = new ParserThread(threadColumns[i], errorReasons[i], i,
			                          numThreads, pStrStart, numCols,hasHeaders, maxLineLength,
				                  columnSpec, interrupt);

		for (int i = 0 ; i < numThreads ; i++)
			parserThreads[i]->Start();

		// Wait until everyone's done
		for (int i = 0 ; i < numThreads ; i++)
			parserThreads[i]->m_endMutex.Lock();
		for (int i = 0 ; i < numThreads ; i++)
			parserThreads[i]->m_endMutex.Unlock();

		// Check if an error was encountered
		for (int i = 0 ; i < numThreads ; i++)
		{
			if (errorReasons[i].length() > 0)
				Throw(errorReasons[i].c_str());
		}

		int totalEntries = 0;
		for (int i = 0 ; i < numThreads ; i++)
		{
			// Each column will have the same number of entries, except for columns which are
			// ignored
			totalEntries += threadColumns[i][lastRealColumn].getEntries(); 
		}

		Rcout << "Read " << totalEntries << " data lines" << endl;

		CharacterVector nameVec;
		for (size_t i = 0 ; i < numCols ; i++)
		{
			if (!threadColumns[0][i].ignore()) // It's ignored in all threads
			{
				//if (hasHeaders)
				nameVec.push_back(names[i]);

				for (int t = 0 ; t < numThreads ; t++)
					threadColumns[t][i].addColumnToList(listOfVectors, numThreads, t, totalEntries);
			}
		}

		//if (hasHeaders)
		listOfVectors.attr("names") = nameVec;

		// Clean up threads
		for (int i = 0 ; i < numThreads ; i++)
		{
			while (parserThreads[i]->IsRunning())
				usleep(10);
			delete parserThreads[i];
		}
#endif // !_WIN32
	}

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

void ValueVector::addColumnToList(List &listOfVectors, int numThreads, int thread, int totalEntries)
{
	// thread 0 should create the new vector, the other threads should use the last added vector

	switch(m_vectorType)
	{
	case Ignore:
		throw Rcpp::exception("Internal error: 'Ignore' should not be used in addColumnToList");
	case Integer:
		{
			if (thread == 0)
				listOfVectors.push_back(IntegerVector(totalEntries));

			IntegerVector v = listOfVectors[listOfVectors.size()-1];

			const int num = m_vectorInt.size();
			int outPos = thread;

			for (int i = 0 ; i < num ; i++, outPos += numThreads)
				v[outPos] = m_vectorInt[i];
		}
		break;
	case Double:
		{
			if (thread == 0)
				listOfVectors.push_back(NumericVector(totalEntries));

			NumericVector v = listOfVectors[listOfVectors.size()-1];

			const int num = m_vectorDouble.size();
			int outPos = thread;

			for (int i = 0 ; i < num ; i++, outPos += numThreads)
				v[outPos] = m_vectorDouble[i];
		}
		break;
	case String:
		{
			if (thread == 0)
				listOfVectors.push_back(StringVector(totalEntries));

			StringVector v = listOfVectors[listOfVectors.size()-1];

			const int num = m_vectorString.size();
			int outPos = thread;

			for (int i = 0 ; i < num ; i++, outPos += numThreads)
				v[outPos] = m_vectorString[i];
		}
		break;
	default:
		throw Rcpp::exception("Internal error: unknown m_vectorType in addColumnToList2");
	}
}

inline char *StrTok(char *pStr, char delim, char **pSavePtr)
{
	char *pStart = pStr;
	if (pStr == 0)
		pStart = *pSavePtr;

	char *pPos = pStart;
	while (1)
	{
		char c = *pPos;

		if (c == delim)
		{
			*pPos = 0;
			*pSavePtr = pPos+1;
			return pStart;
		}
		if (c == '\0')
		{
			*pSavePtr = pPos;
			if (pPos == pStart)
				return NULL;
			return pStart;
		}
		pPos++;
	}

	return NULL; // won't get here
}

#ifndef _WIN32
void ParserThread::runThread()
{
	vector<char> buffer(maxLineLength+1);
	char *buff = &(buffer[0]);

	bool done = false;
	for (int i = 0 ; i < threadNum ; i++) // skip a number of lines, so that each thread reads different lines
	{
		pStr = gotoNextLine(pStr);
		if (*pStr == '\0')
		{
			done = true;
			break;
		}
		lineNumber++;
	}

	while (!done && !interrupt)
	{
		char *pEnd = gotoNextLine(pStr);
		size_t lineLen = pEnd - pStr;

		if (lineLen > maxLineLength)
			lineLen = maxLineLength;

		memcpy(buff, pStr, lineLen);
		buff[lineLen] = 0;

		char *pBuff = buff;
		char *pPtr = 0;

		for (int i = 0 ; !done && i < numCols ; i++)
		{
			char *pPart = StrTok(pBuff, ',', &pPtr);
			pBuff = 0;

			if (!pPart)
			{
				done = true;
				errorString = getString("Not enough columns on line %d", lineNumber);
				interrupt = true;
				break;
			}

			int colNum = i+1;
			if (!columns[i].processWithCheck(pPart, colNum == numCols))
			{
				done = true;
				errorString= getString("Unable to interpret '%s' (line %d, col %d) as type '%c'",
					               pPart, lineNumber, colNum, columnSpec[i]);
				interrupt = true;
			}
		}

		lineNumber++;

		for (int i = 1 ; !done && i < numThreads ; i++) // skip threads-1 lines
		{
			pEnd = gotoNextLine(pEnd);
			if (*pEnd == '\0')
			{
				done = true;
				break;
			}
			lineNumber++;
		}
		pStr = pEnd;
	}

	//Rcout << "Lines read: " << lineNumber << endl;
}

void *ParserThread::Thread()
{
	m_endMutex.Lock();
	JThread::ThreadStarted();

	runThread();
	m_endMutex.Unlock();
	return 0;
}
#endif // !_WIN32
