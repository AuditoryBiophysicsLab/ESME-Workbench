#ifndef _3MBSBATHYMETRYBITMAPWINDOWPROC_H
#define _3MBSBATHYMETRYBITMAPWINDOWPROC_H

#include <windows.h>
#include <commctrl.h>
#include <winuser.h>
#include "3mb.h"
#include "3mbBitmapFunctions.h"
#include "3mbGuiFunctions.h"
#include "3mbScaleBitmapWinProc.h"
#include "resource.h"

/* It helps to keep in mind the following:

					Index.
	y   
0  \|/   90   91   92   93   94   95   96   97   98   99
1	i    80   81   82   83   84   85   86   87   88   89
2	n    70   71   72   73   74   75   76   77   78   79
3	c    60   61   62   63   64   65   66   67   68   69
4	r    50   51   52   53   54   55   56   57   58   59
5	e    40   41   42   43   44   45   46   47   48   49
6	a    30   31   32   33   34   35   36   37   38   39
7   s    20   21   22   23   24   25   26   27   28   29
8   e    10   11   12   13   14   15   16   17   18   19
9	s	  0    1    2    3    4    5    6    7    8    9

	 x->increases ......................................
          0    1    2    3    4    5    6    7    8    9
*/

LRESULT CALLBACK BathyBitmapWindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);


#endif
