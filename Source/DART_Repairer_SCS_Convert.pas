unit DART_Repairer_SCS_Convert;

{$INCLUDE DART_defs.inc}

interface

uses
  Classes,
  DART_Repairer_SCS;

type
  TDARTRepairer_SCS_Convert = class(TDARTRepairer_SCS_ProcessingBase)
  protected
    fConvertedArchiveStream:  TStream;
    procedure ConvertArchiveStructure; virtual; abstract;
    procedure ConvertArchive; virtual; abstract;
    procedure ArchiveProcessing; override;
  end;

implementation

procedure TDARTRepairer_SCS_Convert.ArchiveProcessing;
begin
inherited;
ConvertArchiveStructure;
ConvertArchive;
end;

end.
