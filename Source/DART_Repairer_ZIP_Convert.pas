unit DART_Repairer_ZIP_Convert;

{$INCLUDE DART_defs.inc}

interface

uses
  Classes,
  DART_Repairer_ZIP;

type
  TDARTRepairer_ZIP_Convert = class(TDARTRepairer_ZIP_ProcessingBase)
  protected
    fConvertedArchiveStream:  TStream;
    procedure ConvertArchiveStructure; virtual; abstract;
    procedure ConvertArchive; virtual; abstract;
    procedure ArchiveProcessing; override;
  end;  

implementation

procedure TDARTRepairer_ZIP_Convert.ArchiveProcessing;
begin
inherited;
ConvertArchiveStructure;
ConvertArchive;
end;


end.
