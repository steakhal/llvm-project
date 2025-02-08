#include "AnalysisExtension.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclID.h"
#include "clang/Basic/DiagnosticFrontend.h"
#include "clang/Serialization/ASTBitCodes.h"
#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ASTWriter.h"
#include "clang/Serialization/ModuleFileExtension.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Bitstream/BitstreamReader.h"
#include "llvm/Bitstream/BitstreamWriter.h"
#include "llvm/Support/Error.h"

using namespace clang;

char AnalysisExtension::ID = 0;

class Writer final : public ModuleFileExtensionWriter {
public:
  Writer(ModuleFileExtension *Ext, ASTWriter &W)
      : ModuleFileExtensionWriter(Ext), W(W) {}

  void writeExtensionContents(Sema &SemaRef,
                              llvm::BitstreamWriter &Stream) override;

private:
  ASTWriter &W;
};

class Reader final : public ModuleFileExtensionReader {
public:
  Reader(ModuleFileExtension *Ext, const llvm::BitstreamCursor &Stream,
         ASTReader &R, serialization::ModuleFile &Mod);

private:
  llvm::BitstreamCursor Stream;
  ASTReader &R;
  serialization::ModuleFile &Mod;
};

std::unique_ptr<ModuleFileExtensionWriter>
AnalysisExtension::createExtensionWriter(ASTWriter &W) {
  llvm::errs() << "createExtensionWriter\n";
  return std::make_unique<Writer>(this, W);
}

std::unique_ptr<ModuleFileExtensionReader>
AnalysisExtension::createExtensionReader(
    const ModuleFileExtensionMetadata &Metadata, ASTReader &R,
    serialization::ModuleFile &Mod, const llvm::BitstreamCursor &Stream) {
  llvm::errs() << "createExtensionReader\n";
  assert(Metadata.BlockName == BlockName && "Wrong block name");
  if (Metadata.MajorVersion != MajorVersion ||
      Metadata.MinorVersion != MinorVersion) {
    R.getDiags().Report(Mod.ImportLoc,
                        diag::err_test_module_file_extension_version)
        << BlockName << Metadata.MajorVersion << Metadata.MinorVersion
        << MajorVersion << MinorVersion;
    return nullptr;
  }

  return std::make_unique<Reader>(this, Stream, R, Mod);
}

llvm::SmallVector<const CXXRecordDecl *> Records;



Reader::Reader(ModuleFileExtension *Ext, const llvm::BitstreamCursor &InStream,
               ASTReader &R, serialization::ModuleFile &Mod)
    : ModuleFileExtensionReader(Ext), Stream(InStream), R(R), Mod(Mod) {
  llvm::errs() << "Reader ctor reads\n";

  SmallVector<uint64_t, 64> Record;
  while (true) {
    llvm::Expected<llvm::BitstreamEntry> Entry =
        Stream.advanceSkippingSubblocks();
    cantFail(Entry.takeError()); // TODO: Check for error.

    if (Entry->Kind != llvm::BitstreamEntry::Record)
      return;

    Record.clear();
    Expected<unsigned> RecCode = Stream.readRecord(llvm::bitc::UNABBREV_RECORD, Record);
    cantFail(RecCode.takeError());

    if (*RecCode == 111) {
      for (uint64_t v : Record) {
        LocalDeclID ID = LocalDeclID::get(R, Mod, v);
        Records.push_back(R.GetLocalDeclAs<CXXRecordDecl>(Mod, ID));
        llvm::errs() << "> " << ID.getRawValue() << ", as ";
        llvm::errs() << Records.back() << "\n";
      }
    }

    //llvm::errs() << "extension read len of " << Record[0] << ": '" << Message << "'\n";
  }
}

void Writer::writeExtensionContents(Sema &SemaRef,
                                    llvm::BitstreamWriter &Stream) {
  using namespace llvm;
  llvm::errs() << "writeExtensionContents\n";

  const auto *TU = SemaRef.getASTContext().getTranslationUnitDecl();

  SmallVector<uint64_t, 64> Record;

  for (const auto *D : TU->decls()) {
    if (const auto *FD = dyn_cast<FunctionDecl>(D);
        FD && FD->getNameAsString() == "remote" && false) {
      FD->dumpColor();
      Record.push_back(W.getDeclID(FD).getRawValue());
      llvm::errs() << "decl ID: " << W.getDeclID(FD).getRawValue() << "\n";
    }

    if (const auto *R = dyn_cast<CXXRecordDecl>(D)) {
      R->dumpColor();
      Record.push_back(W.getDeclID(R).getRawValue());
      llvm::errs() << "decl ID: " << W.getDeclID(R).getRawValue() << "\n";
    }
  }

  // TU->dumpColor();
  // auto *Ext = static_cast<AnalysisExtension *>(getExtension());

  // Write a message into the extension block.
  Stream.EmitRecord(111, Record);
  llvm::errs() << "Written: 2 decls\n";
}
