#ifndef BROWSEFOLDER_H
#define BROWSEFOLDER_H

#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <shlobj.h>   
BOOL BrowseFolder(TCHAR *Title, TCHAR PathBuffer[MAX_PATH]);


#endif // BROWSEFOLDER_H