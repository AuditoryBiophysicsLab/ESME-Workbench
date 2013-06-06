using System;
using System.Globalization;
using System.Linq.Expressions;
using System.Windows.Data;
using System.Windows.Markup;

namespace HRC.WPF
{
    public static class Generic
    {
        public static TService GetService<TService>(this IServiceProvider isp)
            where TService : class
        {
            return isp.GetService(typeof (TService))
                   as TService;
        }
    }

    public class LambdaExtension : MarkupExtension
    {
        public string Forward { get; set; }
        public string Backward { get; set; }

        public LambdaExtension() { }

        public LambdaExtension(string forward) { Forward = forward; }

        public override object ProvideValue(IServiceProvider isp)
        {
            var tp = isp.GetService<IProvideValueTarget>();
            var tr = isp.GetService<IXamlTypeResolver>();
            if (tp.TargetObject is Binding) return new SingleConverter(Forward, Backward, tr);
            if (tp.TargetObject is MultiBinding) return new MultiConverter(Forward, Backward, tr);
            if (tp.TargetObject is BindingExtension) return new MultiConverter(Forward, Backward, tr);
            return null;
        }
    }

    class SingleConverter : IValueConverter
    {
        public Delegate ForwardOperation { get; private set; }
        public Delegate BackwardOperation { get; private set; }
        public string Forward { get; private set; }
        public string Backward { get; private set; }
        readonly IXamlTypeResolver _typeResolver;

        public SingleConverter(string forward, string backward, IXamlTypeResolver tr)
        {
            Forward = forward;
            Backward = backward;
            _typeResolver = tr;
        }

        #region IValueConverter Members
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if (ForwardOperation == null)
            {
                if (value == null) return Binding.DoNothing;
                ForwardOperation = ConstructOperation(Forward, value, targetType);
            }
            return ForwardOperation.DynamicInvoke(value);
        }

        Delegate ConstructOperation(string code, object value, Type targetType)
        {
            var opi = code.IndexOf("=>");
            if (opi < 0) throw new Exception("No lambda operator =>");
            var param = code.Substring(0, opi);
            var body = code.Substring(opi + 2);
            var p = Expression.Parameter(value.GetType(), param);
            var lambda = DynamicExpression.ParseLambda(_typeResolver, new[] {p}, targetType, body, value);
            return lambda.Compile();
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if (Backward == null) throw new Exception("No expression for ConvertBack");

            if (BackwardOperation == null)
            {
                if (value == null) return Binding.DoNothing;
                BackwardOperation = ConstructOperation(Backward, value, targetType);
            }
            return BackwardOperation.DynamicInvoke(value);
        }
        #endregion
    }

    class MultiConverter : IMultiValueConverter
    {
        public Delegate ForwardOperation { get; private set; }
        public Delegate BackwardOperation { get; private set; }
        public string Forward { get; private set; }
        public string Backward { get; private set; }
        readonly IXamlTypeResolver _typeResolver;

        public MultiConverter(string forward, string backward, IXamlTypeResolver tr)
        {
            Forward = forward;
            Backward = backward;
            _typeResolver = tr;
        }

        #region IMultiValueConverter Members
        public object Convert(object[] values, Type targetType, object parameter, CultureInfo culture)
        {
            if (ForwardOperation == null)
            {
                var paramSideLength = Forward.IndexOf("=>");
                if (paramSideLength < 0) throw new Exception("No lambda operator =>");
                var paramSide = Forward.Substring(0, paramSideLength);
                var paramNames = paramSide.Split(',');
                var bodySide = Forward.Substring(paramSideLength + 2);
                var pes = new ParameterExpression[paramNames.Length];
                for (var i = 0; i < values.Length; i++) pes[i] = Expression.Parameter(values[i].GetType(), paramNames[i]);
                var lambda = DynamicExpression.ParseLambda(_typeResolver, pes, targetType, bodySide, values);
                ForwardOperation = lambda.Compile();
            }
            return ForwardOperation.DynamicInvoke(values);
        }

        public object[] ConvertBack(object value, Type[] targetTypes, object parameter, CultureInfo culture)
        {
            if (BackwardOperation == null)
            {
                var opi = Backward.IndexOf("=>");
                if (opi < 0) throw new Exception("No lambda operator =>");
                var param = Backward.Substring(0, opi);
                var body = Backward.Substring(opi + 2);
                var p = Expression.Parameter(value.GetType(), param);
                var lambda = DynamicExpression.ParseLambda(_typeResolver, new[] {p}, typeof (object[]), body, value);
                BackwardOperation = lambda.Compile();
            }
            return BackwardOperation.DynamicInvoke(value) as object[];
        }
        #endregion
    }
}