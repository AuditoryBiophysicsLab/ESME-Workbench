using System;
using System.Runtime.Serialization;

namespace ESME.Plugins
{
    public class PluginNotFoundException : Exception
    {
        public PluginNotFoundException() { }
        public PluginNotFoundException(string message) : base(message) { }
        public PluginNotFoundException(string message, Exception innerException) : base(message, innerException) { }
        public PluginNotFoundException(SerializationInfo info, StreamingContext context) : base(info, context) { }
    }
}
