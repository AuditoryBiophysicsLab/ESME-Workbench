#ifndef MBSSPECIESMODEL_H
#define MBSSPECIESMODEL_H

#include "3mb.h"
#include "datatypes.h"

void SpeciesToGuiWindows(CSpeciesModel *sm, HWND parent);
LRESULT CALLBACK SpeciesModelProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam);
void GuiWindowsToSpecies(CSpeciesModel *sm, HWND parent);

#endif //3MBSSPECIESMODEL_H
