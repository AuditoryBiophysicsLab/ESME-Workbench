﻿using System.Collections.Generic;

namespace OneNavyModel.ViewModels.RecentFiles
{
    public interface IPersist
    {
        List<string> RecentFiles(int max);
        void InsertFile(string filepath, int max);
        void RemoveFile(string filepath, int max);
    }
}