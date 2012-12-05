using System;
using System.Collections.Generic;
using System.ComponentModel.Composition.Primitives;
using System.Linq;

namespace HRC.Composition
{
    public abstract class FilteredCatalog : ComposablePartCatalog
    {
        private readonly ComposablePartCatalog _catalogToFilter;

        protected FilteredCatalog(ComposablePartCatalog catalogToFilter)
        {
            _catalogToFilter = catalogToFilter;
        }

        public override IQueryable<ComposablePartDefinition> Parts
        {
            get
            {
                return from part in _catalogToFilter.Parts
                       from exportDefinition in part.ExportDefinitions
                       where IsMatch(part) && IsMatch(exportDefinition)
                       select part;
            }
        }

        public override IEnumerable<Tuple<ComposablePartDefinition, ExportDefinition>> GetExports(ImportDefinition definition)
        {
            return from export in base.GetExports(definition)
                   where IsMatch(export.Item1) && IsMatch(export.Item2)
                   select export;
        }

        protected virtual bool IsMatch(ComposablePartDefinition composablePartDefinition) { return true; }

        protected virtual bool IsMatch(ExportDefinition exportDefinition) { return true; }
    }
}
