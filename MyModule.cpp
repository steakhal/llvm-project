// TODO: license

#include "clang/Basic/DiagnosticFrontend.h"
#include "clang/Serialization/ASTBitCodes.h"
#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ModuleFileExtension.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Bitstream/BitstreamReader.h"
#include "llvm/Bitstream/BitstreamWriter.h"
#include "llvm/Support/Error.h"

using namespace clang;
using namespace llvm;

/// A module file extension used for testing purposes.
class AnalysisExtension final
    : public llvm::RTTIExtends<AnalysisExtension, ModuleFileExtension> {
  static constexpr llvm::StringLiteral BlockName = "analysis-extension";
  static constexpr llvm::StringLiteral UserInfo = "some user info?";
  static constexpr auto MajorVersion = 1;
  static constexpr auto MinorVersion = 1;

public:
  static char ID;

  AnalysisExtension() = default;

  ModuleFileExtensionMetadata getExtensionMetadata() const override {
    return {BlockName.str(), MajorVersion, MinorVersion, UserInfo.str()};
  }

  void hashExtension(ExtensionHashBuilder &HBuilder) const override {
    HBuilder.add(BlockName);
    HBuilder.add(MajorVersion);
    HBuilder.add(MinorVersion);
    HBuilder.add(UserInfo);
  }

  std::unique_ptr<ModuleFileExtensionWriter>
  createExtensionWriter(ASTWriter &Writer) override;

  std::unique_ptr<ModuleFileExtensionReader>
  createExtensionReader(const ModuleFileExtensionMetadata &Metadata,
                        ASTReader &Reader, serialization::ModuleFile &Mod,
                        const llvm::BitstreamCursor &Stream) override;

  std::string str() const {
    std::string Buffer;
    llvm::raw_string_ostream OS(Buffer);
    OS << BlockName << ": v" << MajorVersion << "." << MinorVersion
       << ", user info: " << UserInfo;
    return Buffer;
  }
};

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
         ASTReader &R);

private:
  llvm::BitstreamCursor Stream;
  ASTReader &R;
};

std::unique_ptr<ModuleFileExtensionWriter>
AnalysisExtension::createExtensionWriter(ASTWriter &W) {
  return std::make_unique<Writer>(this, W);
}

std::unique_ptr<ModuleFileExtensionReader>
AnalysisExtension::createExtensionReader(
    const ModuleFileExtensionMetadata &Metadata, ASTReader &R,
    serialization::ModuleFile &Mod, const llvm::BitstreamCursor &Stream) {
  assert(Metadata.BlockName == BlockName && "Wrong block name");
  if (Metadata.MajorVersion != MajorVersion ||
      Metadata.MinorVersion != MinorVersion) {
    R.getDiags().Report(Mod.ImportLoc,
                        diag::err_test_module_file_extension_version)
        << BlockName << Metadata.MajorVersion << Metadata.MinorVersion
        << MajorVersion << MinorVersion;
    return nullptr;
  }

  return std::make_unique<Reader>(this, Stream, R);
}

Reader::Reader(ModuleFileExtension *Ext, const llvm::BitstreamCursor &InStream,
               ASTReader &R)
    : ModuleFileExtensionReader(Ext), Stream(InStream), R(R) {

  // Read the extension block.
  SmallVector<uint64_t, 4> Record;
  while (true) {
    llvm::Expected<llvm::BitstreamEntry> Entry =
        Stream.advanceSkippingSubblocks();
    cantFail(Entry.takeError()); // TODO: Check for error.

    if (Entry->Kind != llvm::BitstreamEntry::Record)
      return;

    Record.clear();
    StringRef Blob;
    Expected<unsigned> RecCode = Stream.readRecord(Entry->ID, Record, &Blob);
    cantFail(RecCode.takeError());

    if (RecCode.get() != serialization::FIRST_EXTENSION_RECORD_ID)
      return;

    StringRef Message = Blob.substr(0, Record[0]);
    llvm::errs() << "extension read: " << Message << "\n";
  }
}

void Writer::writeExtensionContents(Sema &SemaRef,
                                    llvm::BitstreamWriter &Stream) {
  using namespace llvm;

  // Write an abbreviation for this record.
  auto Abv = std::make_shared<llvm::BitCodeAbbrev>();
  Abv->Add(BitCodeAbbrevOp(serialization::FIRST_EXTENSION_RECORD_ID));
  Abv->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 6)); // # of characters
  Abv->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Blob));   // message
  auto Abbrev = Stream.EmitAbbrev(std::move(Abv));

  // Write a message into the extension block.
  SmallString<64> Message;
  {
    auto *Ext = static_cast<AnalysisExtension *>(getExtension());
    auto Info = Ext->getExtensionMetadata();
    raw_svector_ostream OS(Message);
    OS << "Hello from " << Info.BlockName << " v" << Info.MajorVersion << "."
       << Info.MinorVersion << ", user info: " << Info.UserInfo;
  }
  uint64_t Record[]{serialization::FIRST_EXTENSION_RECORD_ID, Message.size()};
  Stream.EmitRecordWithBlob(Abbrev, Record, Message);
  llvm::errs() << "Written: " << Message << "\n";
}
