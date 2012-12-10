using System;
using System.Windows.Data;
using System.Windows.Markup;
using System.Collections.Generic;

namespace HRC.WPF
{
    public class ElementExtension : MarkupExtension
    {
        public ElementExtension(string nameAndSimplePath)
        {
            _binding = new Binding();
            // TODO: implement more property path styles
            var pi = nameAndSimplePath.IndexOf('.');
            _binding.ElementName = nameAndSimplePath.Substring(0, pi);
            _binding.Path = new System.Windows.PropertyPath(nameAndSimplePath.Substring(pi + 1, nameAndSimplePath.Length - pi - 1));
        }

        public override object ProvideValue(IServiceProvider serviceProvider)
        {
            return _binding.ProvideValue(serviceProvider);
        }

        private readonly Binding _binding;
    }

    public class BindingExtension : MarkupExtension
    {
        public BindingExtension()
        {
            _multiBinding = new MultiBinding();
            _parameters = new List<string>();
        }

        #region Binding Parameters
        public Binding a { set { AddBinding("a", value); } }
        public Binding b { set { AddBinding("b", value); } }
        public Binding c { set { AddBinding("c", value); } }
        public Binding d { set { AddBinding("d", value); } }
        public Binding e { set { AddBinding("e", value); } }
        public Binding f { set { AddBinding("f", value); } }
        public Binding g { set { AddBinding("g", value); } }
        public Binding h { set { AddBinding("h", value); } }
        public Binding i { set { AddBinding("i", value); } }
        public Binding j { set { AddBinding("j", value); } }
        public Binding k { set { AddBinding("k", value); } }
        public Binding l { set { AddBinding("l", value); } }
        public Binding m { set { AddBinding("m", value); } }
        public Binding n { set { AddBinding("n", value); } }
        public Binding o { set { AddBinding("o", value); } }
        public Binding p { set { AddBinding("p", value); } }
        public Binding q { set { AddBinding("q", value); } }
        public Binding r { set { AddBinding("r", value); } }
        public Binding s { set { AddBinding("s", value); } }
        public Binding t { set { AddBinding("t", value); } }
        public Binding u { set { AddBinding("u", value); } }
        public Binding v { set { AddBinding("v", value); } }
        public Binding w { set { AddBinding("w", value); } }
        public Binding x { set { AddBinding("x", value); } }
        public Binding y { set { AddBinding("y", value); } }
        public Binding z { set { AddBinding("z", value); } }
        #endregion

        public string Forward
        {
            set
            {
                _forwardExpression = String.Concat(
                    String.Join(",", _parameters.ToArray()),
                    "=>",
                    value);
            }
        }

        public string ConvertBack
        {
            set
            {
                _backwardExpression = String.Concat(
                    String.Join(",", _parameters.ToArray()),
                    "=>",
                    value);
            }
        }

        public override object ProvideValue(IServiceProvider serviceProvider)
        {
            var tr = serviceProvider.GetService(typeof(IXamlTypeResolver)) as IXamlTypeResolver;
            //var tp = serviceProvider.GetService(typeof(IProvideValueTarget)) as IProvideValueTarget;

            _multiBinding.Converter = new MultiConverter(_forwardExpression, _backwardExpression, tr);
            var value = _multiBinding.ProvideValue(serviceProvider);
            return value;
        }

        private void AddBinding(string parameter, BindingBase value)
        {
            _parameters.Add(parameter);
            _multiBinding.Bindings.Add(value);
        }

        private readonly MultiBinding _multiBinding;
        private string _forwardExpression;
        private string _backwardExpression;
        private readonly List<string> _parameters;

    }
}
