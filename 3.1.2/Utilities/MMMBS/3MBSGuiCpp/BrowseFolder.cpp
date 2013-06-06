#include "BrowseFolder.h"

/*
Main credit for implementing the search/browser folder goes to Jered McFerron (JHawkZZ) at the Code Project website.
http://www.codeproject.com/KB/dialog/searchfolders.aspx on 6/11/2009

Modifications as needed by Matt Cross
*/


//This is needed for virtually everything in BrowseFolder.
void SearchFolder(TCHAR *path);

//BROWSE FOLDER - Opens a browse folder dialog.
BOOL BrowseFolder(TCHAR *Title, TCHAR PathBuffer[MAX_PATH])
{
    //TCHAR PathBuffer[MAX_PATH];
    BROWSEINFO bi = {0};
    bi.lpszTitle = Title;
    LPITEMIDLIST pidl = SHBrowseForFolder ( &bi );

    if(pidl == NULL)
		return FALSE;

    // get the name of the folder and put it in path
    SHGetPathFromIDList(pidl, PathBuffer);

    //Set the current directory to path
    //SetCurrentDirectory(PathBuffer);

    //Begin the search
	//SearchFolder(PathBuffer);

    // free memory used
    IMalloc *imalloc = 0;
    if(SUCCEEDED(SHGetMalloc(&imalloc)))
    {
        imalloc->Free(pidl);
        imalloc->Release();
    }

	return TRUE;
}//BROWSE FOLDER


void SearchFolder(TCHAR *path) 
{     
    //Declare all needed handles     
    WIN32_FIND_DATA FindFileData;     
    HANDLE hFind;     
    //TCHAR filename[ MAX_PATH + 256 ];     
    TCHAR pathbak[ MAX_PATH ];     

    //Make a backup of the directory the user chose         
    strcpy_s(pathbak, sizeof(pathbak)/sizeof(TCHAR), path);

    //Find the first file in the directory the user chose     
    hFind = FindFirstFile("*.*", &FindFileData );

    //Use a do/while so we process whatever FindFirstFile returned     
    do     
    {         
        //Is it valid?         
        if(hFind != INVALID_HANDLE_VALUE )         
        {             
            // Is it a . or .. directory? If it is, skip, or we'll go forever.             
            if( 0 == strcmp(FindFileData.cFileName, ".") || 0 == strcmp(FindFileData.cFileName, ".." ))             
                continue;             

            // Restore the original directory chosen by the user             
            strcpy_s(path, sizeof(pathbak)/sizeof(TCHAR), pathbak);

            // Append the file found on to the path of the directory the user chose             
            sprintf_s(path, sizeof(pathbak)/sizeof(TCHAR), "%s\\%s", path, FindFileData.cFileName );

            // If SetCurrentDirectory Succeeds ( returns 1 ) the current file is a
			// directory. Pause this function, and have it call itself. This will begin
			// the whole process over in a sub directory.             

            if(0 != SetCurrentDirectory(path))             
                SearchFolder( path );             

            // Otherwise right here is where you need to insert what you want to do.  As
			// an example let's add the filename to a list box.

            //INSERT WHAT YOU WANT DONE BELOW!             

//            SendMessage( m_listbox_hwnd, LB_ADDSTRING, 0, path );
        }     
    }

    while(FindNextFile(hFind, &FindFileData) && hFind != INVALID_HANDLE_VALUE);

    FindClose ( hFind );
}//SEARCH FOLDER
