// File is modified

/*

    This file is a part of the JThread package, which contains some object-
    oriented thread wrappers for different thread implementations.

    Copyright (c) 2000-2011  Jori Liesenborgs (jori.liesenborgs@gmail.com)

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
    THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.

*/

#ifndef JTHREAD_JMUTEX_H

#define JTHREAD_JMUTEX_H

#ifdef _WIN32
	#include <process.h>
	#include <windows.h>
#else // using pthread
	#include <pthread.h>
#endif // _WIN32

#define ERR_JMUTEX_ALREADYINIT						-1
#define ERR_JMUTEX_NOTINIT						-2
#define ERR_JMUTEX_CANTCREATEMUTEX					-3

namespace jthread
{

class JMutex
{
public:
	JMutex();
	~JMutex();
	int Init();
	int Lock();
	int Unlock();
	bool IsInitialized() 						{ return initialized; }
private:
#ifdef _WIN32
	CRITICAL_SECTION mutex;
#else // pthread mutex
	pthread_mutex_t mutex;
#endif // _WIN32
	bool initialized;
};

} // end namespace

#endif // JTHREAD_JMUTEX_H

