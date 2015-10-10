object fPrcsSettingsForm: TfPrcsSettingsForm
  Left = 60
  Top = 59
  BorderStyle = bsDialog
  Caption = 'Processing settings'
  ClientHeight = 624
  ClientWidth = 944
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblSettingDescription: TLabel
    Left = 576
    Top = 8
    Width = 93
    Height = 13
    Caption = 'Setting description:'
  end
  object grdEndOfCentralDirectory: TGroupBox
    Left = 8
    Top = 200
    Width = 561
    Height = 105
    Caption = 'End of central directory'
    TabOrder = 1
    OnMouseMove = GroupBoxMouseMove
    object bvlEOCDSplit: TBevel
      Left = 8
      Top = 72
      Width = 545
      Height = 9
      Shape = bsTopLine
    end
    object cbIgnoreEndOfCentralDirectory: TCheckBox
      Tag = 20
      Left = 8
      Top = 24
      Width = 169
      Height = 17
      Caption = 'Ignore end of central directory'
      TabOrder = 0
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbIgnoreDiskSplit: TCheckBox
      Tag = 21
      Left = 192
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Ignore disk split'
      TabOrder = 1
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbIgnoreNumberOfEntries: TCheckBox
      Tag = 22
      Left = 376
      Top = 24
      Width = 145
      Height = 17
      Caption = 'Ignore number of entries'
      TabOrder = 2
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbIgnoreCentralDirectoryOffset: TCheckBox
      Tag = 23
      Left = 8
      Top = 48
      Width = 169
      Height = 17
      Caption = 'Ignore central directory offset'
      TabOrder = 3
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbIgnoreComment: TCheckBox
      Tag = 24
      Left = 192
      Top = 48
      Width = 105
      Height = 17
      Caption = 'Ignore comment'
      TabOrder = 4
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbLimitSearch: TCheckBox
      Tag = 25
      Left = 8
      Top = 80
      Width = 185
      Height = 17
      Caption = 'Limit search to one buffer (~1MiB)'
      TabOrder = 5
      OnMouseMove = SettingsMouseMove
    end
  end
  object grbCentralDirectoryHeaders: TGroupBox
    Left = 8
    Top = 312
    Width = 561
    Height = 145
    Caption = 'Central directory headers'
    TabOrder = 2
    OnMouseMove = GroupBoxMouseMove
    object cbCDIgnoreCentralDirectory: TCheckBox
      Tag = 40
      Left = 8
      Top = 24
      Width = 137
      Height = 17
      Caption = 'Ignore central directory'
      TabOrder = 0
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbCDIgnoreSignature: TCheckBox
      Tag = 41
      Left = 192
      Top = 24
      Width = 137
      Height = 17
      Caption = 'Ignore header signature'
      TabOrder = 1
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbCDIgnoreVersions: TCheckBox
      Tag = 42
      Left = 376
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Ignore versions'
      TabOrder = 2
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbCDClearEncryptionFlags: TCheckBox
      Tag = 43
      Left = 8
      Top = 48
      Width = 129
      Height = 17
      Caption = 'Clear encryption flags'
      TabOrder = 3
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbCDIgnoreCompressionMethod: TCheckBox
      Tag = 44
      Left = 192
      Top = 48
      Width = 161
      Height = 17
      Caption = 'Ignore compression method'
      TabOrder = 4
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbCDIgnoreModTime: TCheckBox
      Tag = 45
      Left = 376
      Top = 48
      Width = 121
      Height = 17
      Caption = 'Ignore last mod time'
      TabOrder = 5
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbCDIgnoreModDate: TCheckBox
      Tag = 46
      Left = 8
      Top = 72
      Width = 121
      Height = 17
      Caption = 'Ignore last mod date'
      TabOrder = 6
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbCDIgnoreCRC32: TCheckBox
      Tag = 47
      Left = 192
      Top = 72
      Width = 89
      Height = 17
      Caption = 'Ignore CRC32'
      TabOrder = 7
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbCDIgnoreSizes: TCheckBox
      Tag = 48
      Left = 376
      Top = 72
      Width = 81
      Height = 17
      Caption = 'Ignore sizes'
      TabOrder = 8
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbCDIgnoreInternalFileAttributes: TCheckBox
      Tag = 49
      Left = 8
      Top = 96
      Width = 161
      Height = 17
      Caption = 'Ignore internal file attributes'
      TabOrder = 9
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbCDIgnoreExternalFileAttributes: TCheckBox
      Tag = 50
      Left = 192
      Top = 96
      Width = 169
      Height = 17
      Caption = 'Ignore external file attributes'
      TabOrder = 10
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbCDIgnoreLocalHeaderOffset: TCheckBox
      Tag = 51
      Left = 376
      Top = 96
      Width = 145
      Height = 17
      Caption = 'Ignore local header offset'
      TabOrder = 11
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbCDIgnoreExtraField: TCheckBox
      Tag = 52
      Left = 8
      Top = 120
      Width = 105
      Height = 17
      Caption = 'Ignore extra field'
      TabOrder = 12
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbCDIgnoreFileComment: TCheckBox
      Tag = 53
      Left = 192
      Top = 120
      Width = 121
      Height = 17
      Caption = 'Ignore file comment'
      TabOrder = 13
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
  end
  object grbLocalHeaders: TGroupBox
    Left = 8
    Top = 464
    Width = 561
    Height = 153
    Caption = 'Local headers'
    TabOrder = 3
    OnMouseMove = GroupBoxMouseMove
    object bvlLHSplit: TBevel
      Left = 8
      Top = 120
      Width = 545
      Height = 9
      Shape = bsTopLine
    end
    object cbLHIgnoreSignature: TCheckBox
      Tag = 61
      Left = 192
      Top = 24
      Width = 137
      Height = 17
      Caption = 'Ignore header signature'
      TabOrder = 1
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbLHIgnoreVersions: TCheckBox
      Tag = 62
      Left = 376
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Ignore versions'
      TabOrder = 2
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbLHClearEncryptionFlags: TCheckBox
      Tag = 63
      Left = 8
      Top = 48
      Width = 129
      Height = 17
      Caption = 'Clear encryption flags'
      TabOrder = 3
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbLHIgnoreCompressionMethod: TCheckBox
      Tag = 64
      Left = 192
      Top = 48
      Width = 153
      Height = 17
      Caption = 'Ignore compression method'
      TabOrder = 4
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbLHIgnoreModTime: TCheckBox
      Tag = 65
      Left = 376
      Top = 48
      Width = 121
      Height = 17
      Caption = 'Ignore last mod time'
      TabOrder = 5
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbLHIgnoreModDate: TCheckBox
      Tag = 66
      Left = 8
      Top = 72
      Width = 121
      Height = 17
      Caption = 'Ignore last mod date'
      TabOrder = 6
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbLHIgnoreCRC32: TCheckBox
      Tag = 67
      Left = 192
      Top = 72
      Width = 89
      Height = 17
      Caption = 'Ignore CRC32'
      TabOrder = 7
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbLHIgnoreSizes: TCheckBox
      Tag = 68
      Left = 376
      Top = 72
      Width = 81
      Height = 17
      Caption = 'Ignore sizes'
      TabOrder = 8
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbLHIgnoreExtraField: TCheckBox
      Tag = 70
      Left = 192
      Top = 96
      Width = 113
      Height = 17
      Caption = 'Ignore extra field'
      TabOrder = 10
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbLHIgnoreDataDescriptor: TCheckBox
      Tag = 71
      Left = 8
      Top = 128
      Width = 129
      Height = 17
      Caption = 'Ignore data descriptor'
      TabOrder = 11
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbLHIgnoreLocalHeaders: TCheckBox
      Tag = 60
      Left = 8
      Top = 24
      Width = 121
      Height = 17
      Caption = 'Ignore local headers'
      TabOrder = 0
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbLHIgnoreFileName: TCheckBox
      Tag = 69
      Left = 8
      Top = 96
      Width = 105
      Height = 17
      Caption = 'Ignore file name'
      TabOrder = 9
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
  end
  object btnAccept: TButton
    Left = 736
    Top = 592
    Width = 97
    Height = 25
    Caption = 'Accept'
    TabOrder = 6
    OnClick = btnAcceptClick
  end
  object btnClose: TButton
    Left = 840
    Top = 592
    Width = 97
    Height = 25
    Caption = 'Close'
    TabOrder = 7
    OnClick = btnCloseClick
  end
  object grbGeneral: TGroupBox
    Left = 8
    Top = 8
    Width = 561
    Height = 185
    Caption = 'General settings'
    TabOrder = 0
    OnMouseMove = GroupBoxMouseMove
    object bvlGeneralhorSplit: TBevel
      Left = -176
      Top = 126
      Width = 729
      Height = 9
      Shape = bsTopLine
    end
    object vblGeneralHorSplit_File: TBevel
      Left = 8
      Top = 46
      Width = 545
      Height = 9
      Shape = bsTopLine
    end
    object lblFile: TLabel
      Tag = 1
      Left = 8
      Top = 24
      Width = 26
      Height = 13
      Caption = 'lblFile'
      Constraints.MaxWidth = 393
      OnMouseMove = SettingsMouseMove
    end
    object cbIgnoreFileSignature: TCheckBox
      Tag = 6
      Left = 8
      Top = 136
      Width = 121
      Height = 17
      Caption = 'Ignore file signature'
      TabOrder = 4
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object cbAssumeCompressionMethods: TCheckBox
      Tag = 7
      Left = 192
      Top = 136
      Width = 169
      Height = 17
      Caption = 'Assume compression methods'
      TabOrder = 5
      OnClick = CheckBoxClick
      OnMouseMove = SettingsMouseMove
    end
    object rbRebuild: TRadioButton
      Tag = 2
      Left = 8
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Rebuild archive'
      TabOrder = 0
      OnClick = RepairMethodClick
      OnMouseMove = SettingsMouseMove
    end
    object rbExtract: TRadioButton
      Tag = 3
      Left = 112
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Extract archive'
      TabOrder = 1
      OnClick = RepairMethodClick
      OnMouseMove = SettingsMouseMove
    end
    object lbleData: TLabeledEdit
      Tag = 4
      Left = 8
      Top = 96
      Width = 520
      Height = 21
      EditLabel.Width = 27
      EditLabel.Height = 13
      EditLabel.Caption = 'Data:'
      TabOrder = 2
      OnChange = lbleDataChange
      OnMouseMove = SettingsMouseMove
    end
    object btnBrowse: TButton
      Tag = 5
      Left = 528
      Top = 96
      Width = 25
      Height = 21
      Caption = '...'
      TabOrder = 3
      OnClick = btnBrowseClick
    end
    object cbInMemoryProcessing: TCheckBox
      Tag = 8
      Left = 376
      Top = 136
      Width = 129
      Height = 17
      Caption = 'In memory processing'
      TabOrder = 6
      OnMouseMove = SettingsMouseMove
    end
    object cbIgnoreProcessingErrors: TCheckBox
      Tag = 9
      Left = 8
      Top = 160
      Width = 145
      Height = 17
      Caption = 'Ignore processing errors'
      TabOrder = 7
      OnMouseMove = SettingsMouseMove
    end
  end
  object meSettingDescription: TMemo
    Left = 576
    Top = 24
    Width = 361
    Height = 561
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Move cursor over specific setting to see '
      'its description.')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object btnDefault: TButton
    Left = 576
    Top = 592
    Width = 137
    Height = 25
    Caption = 'Load default settings...'
    TabOrder = 5
    OnClick = btnDefaultClick
  end
  object diaSaveDialog: TSaveDialog
    DefaultExt = '.scs'
    Filter = 
      'SCS mod archive (*.scs)|*.scs|ZIP archive (*.zip)|*.zip|All file' +
      's (*.*)|*.*'
    Left = 536
  end
end
