using System.IO;

namespace HRC
{
    public interface ISerializeDeserialize
    {
        void Serialize(BinaryWriter writer);
        void Deserialize(BinaryReader reader);
    }
}
