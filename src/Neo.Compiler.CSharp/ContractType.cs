using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using System;
using System.Linq;

namespace Neo.Compiler
{
    public enum PrimitiveType : byte
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

    public abstract record ContractType();

    public record ArrayContractType(ContractType Type) : ContractType;
    public record InteropContractType(INamedTypeSymbol Symbol) : ContractType;
    public record MapContractType(PrimitiveType KeyType, ContractType ValueType) : ContractType;
    public record PrimitiveContractType(PrimitiveType Type) : ContractType
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
    public record SymbolContractType(INamedTypeSymbol Symbol) : ContractType;
    public record UnspecifiedContractType() : ContractType
    {
        public readonly static UnspecifiedContractType Unspecified = new UnspecifiedContractType();
    }
    public record VoidContractType() : ContractType
    {
        public readonly static VoidContractType Void = new VoidContractType();
    }

    static class ContractTypeExtensions
    {
        public static string AsString(this ContractType? type)
        {
            type ??= UnspecifiedContractType.Unspecified;
            return type switch
            {
                ArrayContractType a => $"Array<{a.Type.AsString()}>",
                InteropContractType i => $"Interop<{i.Symbol}>",
                MapContractType m => $"Map<#{m.KeyType},{m.ValueType.AsString()}>",
                PrimitiveContractType p => $"#{p.Type}",
                SymbolContractType s => s.Symbol.ToString() ?? throw new Exception(),
                UnspecifiedContractType => "#Unspecified",
                VoidContractType => throw new NotSupportedException($"{nameof(AsString)} {nameof(VoidContractType)}"),
                _ => throw new NotImplementedException($"{nameof(AsString)} {type.GetType().Name}"),
            };
        }
    }

    class Visitor : SymbolVisitor<ContractType>
    {
        readonly Compilation compilation;

        readonly IAssemblySymbol scfx;
        readonly INamedTypeSymbol ecPoint;
        readonly INamedTypeSymbol byteString;
        readonly INamedTypeSymbol uint160;
        readonly INamedTypeSymbol uint256;
        readonly INamedTypeSymbol bigInt;
        readonly INamedTypeSymbol map;
        readonly INamedTypeSymbol list;
        readonly INamedTypeSymbol apiInterface;

        public Visitor(Compilation compilation)
        {
            this.compilation = compilation;

            ecPoint = compilation.GetTypeByMetadataName("Neo.Cryptography.ECC.ECPoint") ?? throw new Exception();
            byteString = compilation.GetTypeByMetadataName("Neo.SmartContract.Framework.ByteString") ?? throw new Exception();
            uint160 = compilation.GetTypeByMetadataName("Neo.UInt160") ?? throw new Exception();
            uint256 = compilation.GetTypeByMetadataName("Neo.UInt256") ?? throw new Exception();
            bigInt = compilation.GetTypeByMetadataName("System.Numerics.BigInteger") ?? throw new Exception();
            map = compilation.GetTypeByMetadataName("Neo.SmartContract.Framework.Map`2") ?? throw new Exception();
            list = compilation.GetTypeByMetadataName("Neo.SmartContract.Framework.List`1") ?? throw new Exception();
            apiInterface = compilation.GetTypeByMetadataName("Neo.SmartContract.Framework.IApiInterface") ?? throw new Exception();

            scfx = uint160.ContainingAssembly;
        }

        public override ContractType? VisitNamedType(INamedTypeSymbol symbol)
        {
            if (symbol.TypeKind == TypeKind.Enum) return PrimitiveContractType.Integer;

            return symbol.SpecialType switch
            {
                SpecialType.System_Boolean => PrimitiveContractType.Boolean,
                SpecialType.System_String => PrimitiveContractType.String,
                SpecialType.System_Void => VoidContractType.Void,
                SpecialType.System_Char => PrimitiveContractType.Integer,
                SpecialType.System_Byte => PrimitiveContractType.Integer,
                SpecialType.System_SByte => PrimitiveContractType.Integer,
                SpecialType.System_Int16 => PrimitiveContractType.Integer,
                SpecialType.System_Int32 => PrimitiveContractType.Integer,
                SpecialType.System_Int64 => PrimitiveContractType.Integer,
                SpecialType.System_UInt16 => PrimitiveContractType.Integer,
                SpecialType.System_UInt32 => PrimitiveContractType.Integer,
                SpecialType.System_UInt64 => PrimitiveContractType.Integer,
                SpecialType.System_Object => UnspecifiedContractType.Unspecified,
                SpecialType.None => ConvertSymbol(symbol),
                _ => throw new NotSupportedException($"SpecialType {symbol.SpecialType}")
            }; 
        }

        public override ContractType? VisitArrayType(IArrayTypeSymbol symbol)
        {
            if (symbol.ElementType.SpecialType == SpecialType.System_Byte)
                return PrimitiveContractType.ByteArray;

            var elementType = Visit(symbol.ElementType) ?? UnspecifiedContractType.Unspecified;
            return new ArrayContractType(elementType);
        }

        ContractType? ConvertSymbol(INamedTypeSymbol symbol)
        {
            var comparer = SymbolEqualityComparer.Default;
            if (comparer.Equals(symbol, bigInt)) return PrimitiveContractType.Integer;
            if (comparer.Equals(symbol, uint160)) return PrimitiveContractType.Hash160;
            if (comparer.Equals(symbol, uint256)) return PrimitiveContractType.Hash256;
            if (comparer.Equals(symbol, byteString)) return PrimitiveContractType.ByteArray;
            if (comparer.Equals(symbol, ecPoint)) return PrimitiveContractType.PublicKey;

            if (symbol.AllInterfaces.Any(i => comparer.Equals(i, apiInterface)))
                return new InteropContractType(symbol);

            if (symbol.IsGenericType)
            {
                if (comparer.Equals(map, symbol.ConstructedFrom))
                {
                    var key = Visit(symbol.TypeArguments[0]) as PrimitiveContractType;
                    if (key is null) throw new Exception("Invalid Map Key Type");

                    var value = Visit(symbol.TypeArguments[1]) ?? UnspecifiedContractType.Unspecified;
                    return new MapContractType(key.Type, value);
                }

                if (comparer.Equals(map, symbol.ConstructedFrom))
                {
                    var type = Visit(symbol.TypeArguments[0]) ?? UnspecifiedContractType.Unspecified;
                    return new ArrayContractType(type);
                }

                return UnspecifiedContractType.Unspecified;
            }

            return new SymbolContractType(symbol);
        }
    }
}