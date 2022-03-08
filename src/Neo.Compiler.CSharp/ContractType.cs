using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using System;

namespace Neo.Compiler
{
    record StructType(string Name) : ContractType;
    
    enum PrimitiveType : byte
    {
        Boolean,
        Integer,
        ByteArray,
        String,
        Hash160,
        Hash256,
        PublicKey,
        Signature,
        Address,
    }

    abstract record ContractType();

    record ArrayContractType(ContractType Type) : ContractType;
    record InteropContractType(INamedTypeSymbol Symbol) : ContractType;
    record MapContractType(PrimitiveType KeyType, ContractType ValueType) : ContractType;
    record NeoScfxContractType(INamedTypeSymbol Symbol) : ContractType;

    record PrimitiveContractType(PrimitiveType Type) : ContractType
    {
        public readonly static PrimitiveContractType Address = new PrimitiveContractType(PrimitiveType.Address);
        public readonly static PrimitiveContractType Boolean = new PrimitiveContractType(PrimitiveType.Boolean);
        public readonly static PrimitiveContractType ByteArray = new PrimitiveContractType(PrimitiveType.ByteArray);
        public readonly static PrimitiveContractType Hash160 = new PrimitiveContractType(PrimitiveType.Hash160);
        public readonly static PrimitiveContractType Hash256 = new PrimitiveContractType(PrimitiveType.Hash256);
        public readonly static PrimitiveContractType Integer = new PrimitiveContractType(PrimitiveType.Integer);
        public readonly static PrimitiveContractType PublicKey = new PrimitiveContractType(PrimitiveType.PublicKey);
        public readonly static PrimitiveContractType Signature = new PrimitiveContractType(PrimitiveType.Signature);
        public readonly static PrimitiveContractType String = new PrimitiveContractType(PrimitiveType.String);
    }
    record SymbolContractType(INamedTypeSymbol Symbol) : ContractType;
    record UnspecifiedContractType() : ContractType
    {
        public readonly static UnspecifiedContractType Unspecified = new UnspecifiedContractType();
    }
    record VoidContractType() : ContractType
    {
        public readonly static VoidContractType Void = new VoidContractType();
    }

    static class ContractTypeExtensions
    {
        public static string AsString(this ContractType type)
        {
            return type switch
            {
                ArrayContractType a => $"Array<{a.Type.AsString()}>",
                InteropContractType i => $"Interop<{i.Symbol}>",
                MapContractType m => $"Map<{m.KeyType}:{m.ValueType.AsString()}>",
                NeoScfxContractType n => $"Neo#{n.Symbol.Name}",
                PrimitiveContractType p => $"#{p.Type}",
                SymbolContractType s => ValidateTypeSource($"{s.Symbol.ContainingSymbol}.{s.Symbol.Name}"),
                UnspecifiedContractType => "#Unspecified",
                VoidContractType => throw new NotSupportedException($"{nameof(AsString)} {nameof(VoidContractType)}"),
                _ => throw new NotImplementedException($"{nameof(AsString)} {type.GetType().Name}"),
            };

            static string ValidateTypeSource(string typeName) 
                => (typeName.Contains('#') || typeName.Contains('<') || typeName.Contains('>'))
                    ? throw new NotSupportedException($"Invalid type name {typeName}")
                    : typeName;
        }
    }
}