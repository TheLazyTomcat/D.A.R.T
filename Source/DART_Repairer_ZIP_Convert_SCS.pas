unit DART_Repairer_ZIP_Convert_SCS;

{$INCLUDE DART_defs.inc}

interface

uses
  DART_Format_SCS, DART_Repairer_ZIP_Convert;

type
  TDARTRepairer_ZIP_Convert_SCS = class(TDARTRepairer_ZIP_Convert)
  protected
    fSCSArchiveStructure: TDART_SCS_ArchiveStructure;
    //procedure ConvertArchiveStructure; override;
    //procedure ConvertArchive; override;
  end;

implementation

end.
