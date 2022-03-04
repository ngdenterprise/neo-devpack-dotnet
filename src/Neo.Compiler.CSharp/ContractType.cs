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
                SymbolContractType s => $"{s.Symbol.ContainingSymbol}.{s.Symbol.Name}",
                UnspecifiedContractType => "#Unspecified",
                VoidContractType => throw new NotSupportedException($"{nameof(AsString)} {nameof(VoidContractType)}"),
                _ => throw new NotImplementedException($"{nameof(AsString)} {type.GetType().Name}"),
            };
        }

        public static INamedTypeSymbol FindType(this Compilation compilation, string name)
            => compilation.GetTypeByMetadataName(name) ?? throw new Exception($"{name} type not found");
    }

    class Visitor : SymbolVisitor<ContractType>
    {
        readonly Compilation compilation;

        readonly IAssemblySymbol scfx;
        readonly INamedTypeSymbol? address;
        readonly INamedTypeSymbol apiInterface;
        readonly INamedTypeSymbol bigInt;
        readonly INamedTypeSymbol byteString;
        readonly INamedTypeSymbol ecPoint;
        readonly INamedTypeSymbol list;
        readonly INamedTypeSymbol map;
        readonly INamedTypeSymbol uint160;
        readonly INamedTypeSymbol uint256;

        public Visitor(Compilation compilation)
        {
            this.compilation = compilation;

            address = compilation.GetTypeByMetadataName("Neo.SmartContract.Framework.Address");
            apiInterface = compilation.FindType("Neo.SmartContract.Framework.IApiInterface");
            bigInt = compilation.FindType("System.Numerics.BigInteger");
            byteString = compilation.FindType("Neo.SmartContract.Framework.ByteString");
            ecPoint = compilation.FindType("Neo.Cryptography.ECC.ECPoint");
            list = compilation.FindType("Neo.SmartContract.Framework.List`1");
            map = compilation.FindType("Neo.SmartContract.Framework.Map`2");
            uint160 = compilation.FindType("Neo.UInt160");
            uint256 = compilation.FindType("Neo.UInt256");

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
            Func<ISymbol?, ISymbol?, bool> equals = SymbolEqualityComparer.Default.Equals;
            if (equals(symbol, address)) return PrimitiveContractType.Address;
            if (equals(symbol, bigInt)) return PrimitiveContractType.Integer;
            if (equals(symbol, byteString)) return PrimitiveContractType.ByteArray;
            if (equals(symbol, ecPoint)) return PrimitiveContractType.PublicKey;
            if (equals(symbol, uint160)) return PrimitiveContractType.Hash160;
            if (equals(symbol, uint256)) return PrimitiveContractType.Hash256;

            if (symbol.AllInterfaces.Any(i => equals(i, apiInterface)))
                return new InteropContractType(symbol);

            if (symbol.IsGenericType)
            {
                if (equals(map, symbol.ConstructedFrom))
                {
                    var key = Visit(symbol.TypeArguments[0]) as PrimitiveContractType;
                    if (key is null) throw new Exception("Invalid Map Key Type");

                    var value = Visit(symbol.TypeArguments[1]) ?? UnspecifiedContractType.Unspecified;
                    return new MapContractType(key.Type, value);
                }

                if (equals(map, symbol.ConstructedFrom))
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