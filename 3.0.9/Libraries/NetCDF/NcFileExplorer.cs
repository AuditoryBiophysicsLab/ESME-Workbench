using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.IO;

namespace NetCDF
{
    public partial class NcFileExplorer : TreeView
    {
        private NcFile ncFile;

        public NcFileExplorer()
        {
        }

        /// <summary>
        /// Explore an NcFile object, viewing global properties, dimensions, and variables
        /// Data contained in the specified NcFile are NOT displayed by this control
        /// </summary>
        /// <param name="NcFile">Previously constructed NcFile object to explore</param>
        private void Initialize()
        {
            TreeNode rootNode, attributesNode, dimensionsNode, variablesNode;

            InitializeComponent();

            this.Enabled = true;
            rootNode = new TreeNode(Path.GetFileName(ncFile.Filename));
            this.Nodes.Add(rootNode);

            attributesNode = new TreeNode(String.Format("Global attributes ({0})", ncFile.AttributeCount));
            rootNode.Nodes.Add(attributesNode);
            foreach (NcComponent curAtt in ncFile.Attributes)
                attributesNode.Nodes.Add(curAtt.ToString());
            attributesNode.Expand();

            dimensionsNode = new TreeNode(String.Format("Dimensions ({0})", ncFile.DimensionCount));
            rootNode.Nodes.Add(dimensionsNode);
            foreach (NcDim curDim in ncFile.Dimensions)
                if (curDim.IsUnlimited)
                    dimensionsNode.Nodes.Add(String.Format("{0}[{1}] (unlimited)", curDim.ComponentName, curDim.Size));
                else
                    dimensionsNode.Nodes.Add(String.Format("{0}[{1}]", curDim.ComponentName, curDim.Size));
            dimensionsNode.Expand();

            variablesNode = new TreeNode(String.Format("Variables ({0})", ncFile.VariableCount));
            rootNode.Nodes.Add(variablesNode);
            foreach (NcVar curVar in ncFile.Variables)
            {
                string varEntry = String.Format("{0} {1}", curVar.NcType.ToString(), curVar.ComponentName);
                for (int i = 0; i < curVar.Dimensions.Count; i++)
                    varEntry += String.Format("[{0}]", curVar.Dimensions[i].ComponentName);
                TreeNode curVarNode = new TreeNode(varEntry);
                curVarNode.Nodes.Add(String.Format("Elements: {0:###,###,###,###,###}", curVar.ElementCount));
                curVarNode.Nodes.Add(String.Format("Size: {0:###,###,###,###,###} (bytes)", curVar.Size));
                TreeNode curVarAttNode = new TreeNode(String.Format("Attributes ({0})", curVar.Attributes.Count));
                foreach (NcAtt curAtt in curVar.Attributes)
                    curVarAttNode.Nodes.Add(curAtt.ToString());
                curVarNode.Nodes.Add(curVarAttNode);
                curVarNode.Expand();
                variablesNode.Nodes.Add(curVarNode);

                // This line causes the entire variable to be read into memory.  Make sure there's enough space, or you'll get an OutOfMemoryException
                //curVar.ReadData();
            }
            variablesNode.Expand();
            rootNode.Expand();
        }

        public NcFile NcFile
        {
            set
            {
                this.Nodes.Clear();
                ncFile = value;
                if (ncFile != null)
                    Initialize();
                else
                    this.Enabled = false;
            }
            get { return ncFile; }
        }
    }
}
