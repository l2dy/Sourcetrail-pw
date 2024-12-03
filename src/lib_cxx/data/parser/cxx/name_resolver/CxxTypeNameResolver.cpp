#include "CxxTypeNameResolver.h"

#include <clang/AST/ASTContext.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/AST/PrettyPrinter.h>

#include "CxxDeclNameResolver.h"
#include "CxxSpecifierNameResolver.h"
#include "CxxTemplateArgumentNameResolver.h"
#include "logging.h"
#include "utilityString.h"

CxxTypeNameResolver::CxxTypeNameResolver(CanonicalFilePathCache* canonicalFilePathCache)
	: CxxNameResolver(canonicalFilePathCache)
{
}

CxxTypeNameResolver::CxxTypeNameResolver(const CxxNameResolver* other): CxxNameResolver(other) {}

std::unique_ptr<CxxTypeName> CxxTypeNameResolver::getName(const clang::QualType& qualType)
{
	std::unique_ptr<CxxTypeName> typeName = getName(qualType.getTypePtr());
	if (typeName && qualType.isConstQualified())
	{
		typeName->addQualifier(CxxQualifierFlags::QUALIFIER_CONST);
	}
	return typeName;
}

std::unique_ptr<CxxTypeName> CxxTypeNameResolver::getName(const clang::Type* type)
{
	if (type)
	{
		switch (type->getTypeClass())
		{
		case clang::Type::Paren:
		{
			return getName(type->getAs<clang::ParenType>()->getInnerType());
		}
		case clang::Type::Attributed:
		{
			return getName(type->getAs<clang::AttributedType>()->getModifiedType());
		}
		case clang::Type::InjectedClassName:
		{
			return getName(
				type->getAs<clang::InjectedClassNameType>()->getInjectedSpecializationType());
		}
		case clang::Type::Typedef:
		{
			std::unique_ptr<CxxDeclName> declName = CxxDeclNameResolver(this).getName(
				type->getAs<clang::TypedefType>()->getDecl());
			if (declName)
			{
				return std::make_unique<CxxTypeName>(
					declName->getName(), std::vector<std::wstring>(), declName->getParent());
			}
			break;
		}
		case clang::Type::MemberPointer:
		case clang::Type::Pointer:
		{
			std::unique_ptr<CxxTypeName> typeName = getName(type->getPointeeType());
			if (typeName)
			{
				typeName->addModifier(CxxTypeName::Modifier(L"*"));
			}
			return typeName;
		}
		case clang::Type::ConstantArray:
		case clang::Type::DependentSizedArray:
		case clang::Type::IncompleteArray:
		case clang::Type::VariableArray:
		{
			std::unique_ptr<CxxTypeName> typeName = getName(
				clang::dyn_cast<clang::ArrayType>(type)->getElementType());
			if (typeName)
			{
				typeName->addModifier(CxxTypeName::Modifier(L"[]"));
			}
			return typeName;
		}
		case clang::Type::LValueReference:
		{
			std::unique_ptr<CxxTypeName> typeName = getName(type->getPointeeType());
			if (typeName)
			{
				typeName->addModifier(CxxTypeName::Modifier(L"&"));
			}
			return typeName;
		}
		case clang::Type::RValueReference:
		{
			std::unique_ptr<CxxTypeName> typeName = getName(type->getPointeeType());
			if (typeName)
			{
				typeName->addModifier(CxxTypeName::Modifier(L"&&"));
			}
			return typeName;
		}
		case clang::Type::Elaborated:
		{
			return getName(clang::dyn_cast<clang::ElaboratedType>(type)->getNamedType());
		}
		case clang::Type::Enum:
		case clang::Type::Record:
		{
			std::unique_ptr<CxxDeclName> declName = CxxDeclNameResolver(this).getName(
				type->getAs<clang::TagType>()->getDecl());
			if (declName)
			{
				return std::make_unique<CxxTypeName>(
					declName->getName(),
					declName->getTemplateParameterNames(),	  // contains template arguments if decl
															  // is a template specialization
					declName->getParent());
			}
			break;
		}
		case clang::Type::Builtin:
		{
			clang::PrintingPolicy pp = clang::PrintingPolicy(clang::LangOptions());
			pp.SuppressTagKeyword =
				true;		   // value "true": for a class A it prints "A" instead of "class A"
			pp.Bool = true;	   // value "true": prints bool type as "bool" instead of "_Bool"

			return std::make_unique<CxxTypeName>(
				utility::decodeFromUtf8(type->getAs<clang::BuiltinType>()->getName(pp).str()),
				std::vector<std::wstring>());
		}
		case clang::Type::TemplateSpecialization:
		{
			const clang::TagType* tagType =
				type->getAs<clang::TagType>();	  // remove this case when NameHierarchy is split
												  // into namepart and parameter part
			if (tagType)
			{
				std::unique_ptr<CxxDeclName> declName = CxxDeclNameResolver(this).getName(
					tagType->getDecl());
				if (declName)
				{
					return std::make_unique<CxxTypeName>(
						declName->getName(),
						declName->getTemplateParameterNames(),
						declName->getParent());
				}
			}
			else	// specialization of a template template parameter (no concrete class)
					// important, may help: has no underlying decl!
			{
				const clang::TemplateSpecializationType* templateSpecializationType =
					type->getAs<clang::TemplateSpecializationType>();
				const std::unique_ptr<CxxDeclName> declName = CxxDeclNameResolver(this).getName(
					templateSpecializationType->getTemplateName().getAsTemplateDecl());

				if (declName)
				{
					std::vector<std::wstring> templateArguments;
					CxxTemplateArgumentNameResolver resolver(this);
					resolver.ignoreContextDecl(templateSpecializationType->getTemplateName()
												   .getAsTemplateDecl()
												   ->getTemplatedDecl());
					for (unsigned i = 0; i < templateSpecializationType->template_arguments().size(); i++)
					{
						if (templateSpecializationType->template_arguments()[i].isDependent())
						{
							return std::make_unique<CxxTypeName>(
								declName->getName(),
								declName->getTemplateParameterNames(),
								declName->getParent());
						}
						templateArguments.push_back(
						        resolver.getTemplateArgumentName(templateSpecializationType->template_arguments()[i]));
					}

					return std::make_unique<CxxTypeName>(
						declName->getName(), std::move(templateArguments), declName->getParent());
				}
				else
				{
					LOG_WARNING("no decl found");
				}
			}
			break;
		}
		case clang::Type::TemplateTypeParm:
		{
			std::unique_ptr<CxxDeclName> declName = CxxDeclNameResolver(this).getName(
				clang::dyn_cast<clang::TemplateTypeParmType>(type)->getDecl());
			if (declName)
			{
				return std::make_unique<CxxTypeName>(
					declName->getName(), declName->getTemplateParameterNames(), declName->getParent());
			}
			break;
		}
		case clang::Type::SubstTemplateTypeParm:
		{
			return getName(type->getAs<clang::SubstTemplateTypeParmType>()->getReplacementType());
		}
		case clang::Type::DependentName:
		{
			const clang::DependentNameType* dependentType =
				clang::dyn_cast<clang::DependentNameType>(type);
			std::unique_ptr<CxxName> specifierName = CxxSpecifierNameResolver(this).getName(
				dependentType->getQualifier());
			return std::make_unique<CxxTypeName>(
				utility::decodeFromUtf8(dependentType->getIdentifier()->getName().str()),
				std::vector<std::wstring>(),
				std::move(specifierName));
		}
		case clang::Type::DependentTemplateSpecialization:
		{
			const clang::DependentTemplateSpecializationType* dependentType =
				clang::dyn_cast<clang::DependentTemplateSpecializationType>(type);
			std::unique_ptr<CxxName> specifierName = CxxSpecifierNameResolver(this).getName(
				dependentType->getQualifier());

			std::vector<std::wstring> templateArguments;
			CxxTemplateArgumentNameResolver resolver(this);
			for (unsigned i = 0; i < dependentType->template_arguments().size(); i++)
			{
				templateArguments.push_back(
				    resolver.getTemplateArgumentName(dependentType->template_arguments()[i]));
			}

			return std::make_unique<CxxTypeName>(
				utility::decodeFromUtf8(dependentType->getIdentifier()->getName().str()),
				std::move(templateArguments),
				std::move(specifierName));
		}
		case clang::Type::PackExpansion:
		{
			return getName(clang::dyn_cast<clang::PackExpansionType>(type)->getPattern());
		}
		case clang::Type::Auto:
		{
			clang::QualType deducedType = clang::dyn_cast<clang::AutoType>(type)->getDeducedType();
			if (!deducedType.isNull())
			{
				return getName(deducedType);
			}

			return std::make_unique<CxxTypeName>(
				L"auto");	 // TODO: can we actually resolve this case? would be great!
		}
		case clang::Type::DeducedTemplateSpecialization:
		{
			clang::QualType deducedType = clang::dyn_cast<clang::DeducedTemplateSpecializationType>(type)->getDeducedType();
			if (!deducedType.isNull())
			{
				return getName(deducedType);
			}

			return std::make_unique<CxxTypeName>(
				L"deducedtemplatespecialization");   // TODO: can we actually resolve this case? would be great!
		}
		case clang::Type::Decltype:
		{
			return getName(clang::dyn_cast<clang::DecltypeType>(type)->getUnderlyingType());
		}
		case clang::Type::FunctionProto:
		{
			const clang::FunctionProtoType* protoType = clang::dyn_cast<clang::FunctionProtoType>(
				type);
			std::wstring nameString =
				CxxTypeName::makeUnsolvedIfNull(getName(protoType->getReturnType()))->toString();
			nameString += L"(";
			for (unsigned i = 0; i < protoType->getNumParams(); i++)
			{
				if (i != 0)
				{
					nameString += L", ";
				}
				nameString +=
					CxxTypeName::makeUnsolvedIfNull(getName(protoType->getParamType(i)))->toString();
			}
			nameString += L")";

			return std::make_unique<CxxTypeName>(std::move(nameString));
		}
		case clang::Type::Adjusted:
		case clang::Type::Decayed:
		{
			return getName(type->getAs<clang::AdjustedType>()->getOriginalType());
		}
		case clang::Type::TypeOfExpr:
		{
			if (const clang::TypeOfExprType *tOfExp = clang::dyn_cast<clang::TypeOfExprType>(type)) {
				const clang::Expr *expr = tOfExp->getUnderlyingExpr();
				clang::QualType exprType = expr->getType();
				return getName(exprType);
			}
			break;
		}
		case clang::Type::Using:
		{
			if (const clang::UsingType *uType = dyn_cast<clang::UsingType>(type)) {
				clang::QualType underlyingType = uType->getUnderlyingType();
				return getName(underlyingType);
			}
			break;
		}
		case clang::Type::UnresolvedUsing:
		{
			if (const clang::UnresolvedUsingType *uType = dyn_cast<clang::UnresolvedUsingType>(type)) {
				const clang::UnresolvedUsingTypenameDecl *decl = uType->getDecl();
				if (decl) {
					std::unique_ptr<CxxDeclName> declName = CxxDeclNameResolver(this).getName(decl);
					if (declName) {
						return std::make_unique<CxxTypeName>(
							declName->getName(), std::vector<std::wstring>(), declName->getParent());
					}
				}
			}
			// fall-below
		}
		default:
		{
			const std::string typeClassName = type->getTypeClassName();
			LOG_INFO("Unhandled kind of type encountered: " + typeClassName);
			clang::PrintingPolicy pp = clang::PrintingPolicy(clang::LangOptions());
			pp.SuppressTagKeyword =
				true;		   // value "true": for a class A it prints "A" instead of "class A"
			pp.Bool = true;	   // value "true": prints bool type as "bool" instead of "_Bool"

			clang::SmallString<64> Buf;
			llvm::raw_svector_ostream StrOS(Buf);
			clang::QualType::print(type, clang::Qualifiers(), StrOS, pp, clang::Twine());
			std::wstring nameString = utility::decodeFromUtf8(StrOS.str().str());

			return std::make_unique<CxxTypeName>(std::move(nameString));
		}
		}
	}
	return nullptr;
}
