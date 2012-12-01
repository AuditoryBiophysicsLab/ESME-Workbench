using System;
using System.Collections.Generic;
using System.Reflection;
using System.Windows;
using System.Windows.Controls;
using HRC.Services;
using HRC.ViewModels;
using MEFedMVVM.ViewModelLocator;

namespace HRC
{
    public static class HRCBootstrapper
    {
        public static void Initialise(ICollection<Assembly> assembliesToExamine)
        {
            try
            {
                //now pass the same Assemblies to the ViewResolver so it can
                //resolve the workspace Types
                ViewResolver.ResolveViewLookups(assembliesToExamine);
                PopupResolver.ResolvePopupLookups(assembliesToExamine);
            }
            catch (Exception ex)
            {
                throw new InvalidOperationException("Bootstrapper.Initialise() failed", ex);
            }
        }
    }
    public static class ViewResolver
    {
        private static readonly Dictionary<string, Type> RegisteredViews = new Dictionary<string, Type>();


        public static void ResolveViewLookups(IEnumerable<Assembly> assemblies)
        {
            try
            {
                foreach (var assembly in assemblies)
                {
                    foreach (var type in assembly.GetTypes())
                    {
                        foreach (var attrib in type.GetCustomAttributes(typeof(ViewnameToViewLookupKeyMetadataAttribute), true))
                        {
                            var viewMetadataAtt = (ViewnameToViewLookupKeyMetadataAttribute)attrib;
                            lock (RegisteredViews) RegisteredViews.Add(viewMetadataAtt.ViewName, viewMetadataAtt.ViewLookupKey);
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                throw new InvalidOperationException("ViewResolver is unable to ResolveViewLookups based on current parameters", ex);
            }
        }

        public static DependencyObject CreateView(string viewName)
        {
            if (string.IsNullOrEmpty(viewName))
                throw new ArgumentNullException("viewName");

            Type viewLookupKey;
            lock (RegisteredViews)
            {
                if (!RegisteredViews.ContainsKey(viewName))
                    throw new InvalidOperationException(
                        String.Format("ViewResolver could not CreateView using Key{0}", viewName));

                if (!RegisteredViews.TryGetValue(viewName, out viewLookupKey))
                    return null;
            }

            return (DependencyObject)Activator.CreateInstance(viewLookupKey);
        }


        public static void Register(Dictionary<string, Type> startupData)
        {
            foreach (var entry in startupData)
                Register(entry.Key, entry.Value);
        }


        public static void Register(string viewName, Type viewLookupKey)
        {
            if (string.IsNullOrEmpty(viewName))
                throw new ArgumentNullException("viewName");

            if (viewLookupKey == null)
                throw new ArgumentNullException("viewLookupKey");

            if (!typeof(UserControl).IsAssignableFrom(viewLookupKey))
                throw new ArgumentException("viewLookupKey must be of UserControl");

            lock (RegisteredViews) RegisteredViews.Add(viewName, viewLookupKey);
        }


        public static bool Unregister(string viewName)
        {
            if (string.IsNullOrEmpty(viewName))
                throw new ArgumentNullException("viewName");

            lock (RegisteredViews) return RegisteredViews.Remove(viewName);
        }
    }

    public static class PopupResolver
    {
        public static void ResolvePopupLookups(IEnumerable<Assembly> assemblies)
        {
            try
            {
                var export = ViewModelRepository.Instance.Resolver.Container.GetExport<IUIVisualizerService>();
                if (export == null) return;
                var uiVisualizerService = export.Value;

                foreach (var assembly in assemblies)
                {
                    foreach (var type in assembly.GetTypes())
                    {
                        foreach (PopupNameToViewLookupKeyMetadataAttribute attribute in type.GetCustomAttributes(typeof (PopupNameToViewLookupKeyMetadataAttribute), true)) 
                            uiVisualizerService.Register(attribute.PopupName, attribute.ViewLookupKey);
                    }
                }
            }
            catch (Exception ex)
            {
                throw new InvalidOperationException("PopupResolver is unable to ResolvePopupLookups based on current parameters", ex);
            }
        }
    }
}
