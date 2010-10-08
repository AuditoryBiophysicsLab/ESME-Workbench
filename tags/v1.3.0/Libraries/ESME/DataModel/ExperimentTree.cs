using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Windows.Input;
using System.IO;
using System.Reflection;
using ESME.Model;

namespace ESME.DataModel
{
    public class ExperimentTreeItem : DataModelBase
    {
        public static readonly ResourceImageList Images = new ResourceImageList(typeof(ExperimentTreeItem), "ESME.DataModel.SmallIcons");

        public Image Image 
        {
            get
            {
                VerifyCalledOnUIThread();
                return _image;
            }

            private set
            {
                VerifyCalledOnUIThread();
                if (_image != value)
                {
                    _image = value;
                    SendPropertyChanged("Image");
                }
            }
        }

        public string Name
        {
            get
            {
                VerifyCalledOnUIThread();
                return _name;
            }

            private set
            {
                VerifyCalledOnUIThread();
                if (_name != value)
                {
                    _name = value;
                    SendPropertyChanged("Name");
                }
            }
        }

        public RoutedCommand RoutedCommand
        {
            get
            {
                VerifyCalledOnUIThread();
                return _routedCommand;
            }

            private set
            {
                VerifyCalledOnUIThread();
                if (_routedCommand != value)
                {
                    _routedCommand = value;
                    SendPropertyChanged("RoutedCommand");
                }
            }
        }

        public string Visibility
        {
            get
            {
                VerifyCalledOnUIThread();
                if (_visible)
                    return "Visible";
                else
                    return "Hidden";
            }
        }

        public List<ExperimentTreeItem> Children { get; private set; }

        internal bool Visible
        {
            get { return _visible; }
            set
            {
                VerifyCalledOnUIThread();
                if (_visible != value)
                {
                    _visible = value;
                    SendPropertyChanged("Visibility");
                }
            }
        }

        public ExperimentTreeItem(string Name, string ImageName)
        {
            System.Diagnostics.Debug.WriteLine("Creating item " + Name + " with image " + ImageName);
            this.Name = Name;
            Children = new List<ExperimentTreeItem>();
            //this.Image = Images[ImageName];
        }

        public void Initialize()
        {
            foreach (ExperimentTreeItem child in Children)
            {
                child._parent = this;
                child.Initialize();
            }
        }

        public static List<ExperimentTreeItem> Create(ESME_Experiment Experiment)
        {
            var Environment = new ExperimentTreeItem("Environment", "XPfolder_Open2")
            {
                Children =
                {
                    new ExperimentTreeItem("Location & SSF", "bathymetry")
                    {
                    },
                    new ExperimentTreeItem("Sediment", "sediment")
                    {
                    },
                    new ExperimentTreeItem("Wind", "wind")
                    {
                    },
                    new ExperimentTreeItem("Property Viewer", "properties")
                    {
                    },
                },
            };
            var SoundSources = new ExperimentTreeItem("Sound Sources", "XPfolder_Open2")
            {
                Children =
                {
                    new ExperimentTreeItem("Add New", "add")
                    {
                    },
                    new ExperimentTreeItem("Delete", "delete")
                    {
                    },
                    new ExperimentTreeItem("Settings", "properties")
                    {
                    },
                },
            };
            var Animals = new ExperimentTreeItem("Animals", "XPfolder_Open2")
            {
                Children =
                {
                    new ExperimentTreeItem("Add New", "add")
                    {
                    },
                    new ExperimentTreeItem("Delete", "delete")
                    {
                    },
                    new ExperimentTreeItem("Settings", "properties")
                    {
                    },
                },
            };
            var Setup = new ExperimentTreeItem("Setup", "XPfolder_Open2")
            {
                Children =
                {
                    Environment,
                    SoundSources,
                    Animals,
                },
            };
            var Simulation = new ExperimentTreeItem("Simulation", "XPfolder_Open2")
            {
                Children =
                {
                    new ExperimentTreeItem("Run Animat Simulation", "play")
                    {
                    },
                    new ExperimentTreeItem("Run Transmission Loss", "play")
                    {
                    },
                    new ExperimentTreeItem("Run Simulation", "play")
                    {
                    },
                },
            };
            var Analysis = new ExperimentTreeItem("Analysis/Visualization", "XPfolder_Open2")
            {
                Children =
                {
                    new ExperimentTreeItem("Location View", "searchWeb")
                    {
                    },
                    new ExperimentTreeItem("View Transmission Loss", "tl")
                    {
                    },
                },
            };

            ExperimentTreeItem root = new ExperimentTreeItem("Experiment '" + Experiment.Information.Name + "'", "Control_PictureBox")
            {
                Children =
                {
                    Setup,
                    Simulation,
                    Analysis,
                }
            };
            root.Initialize();
            return new List<ExperimentTreeItem> { root };
        }

        private Image _image;
        private string _name;
        private RoutedCommand _routedCommand;
        private bool _visible;
        private ExperimentTreeItem _parent;
    }

    public class ResourceImage
    {
        public Image Image { get; private set; }
        public string Name { get; private set; }

        public ResourceImage(string ResourceName)
        {
            string[] fields = ResourceName.Split(ResourceImage._separators, StringSplitOptions.RemoveEmptyEntries);
            Name = fields[fields.Length - 2];
            using (Stream stream = System.Reflection.Assembly.GetCallingAssembly().GetManifestResourceStream(ResourceName))
                Image = new Bitmap(stream);
        }

        private static readonly char[] _separators = { '.' };
    }

    public class ResourceImageList : List<ResourceImage>
    {
        public ResourceImageList(Type type, string StartsWith)
        {
            string[] resources = type.Assembly.GetManifestResourceNames();
            foreach (string r in resources)
            {
                try
                {
                    if ((StartsWith == null) || (StartsWith == "") || (r.StartsWith(StartsWith)))
                        Add(new ResourceImage(r));
                }
                catch
                {
                }
            }
        }

        public Image this[string name]
        {
            get
            {
                return this.Find(r => r.Name == name).Image;
            }
        }
    }
}
