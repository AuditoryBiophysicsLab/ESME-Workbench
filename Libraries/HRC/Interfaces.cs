using System.Windows.Threading;

namespace HRC
{
    public interface IRequireInvoke
    {
        Dispatcher Dispatcher { get; set; }
    }
}
