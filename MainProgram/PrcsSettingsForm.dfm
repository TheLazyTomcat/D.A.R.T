object fPrcsSettingsForm: TfPrcsSettingsForm
  Left = 590
  Top = 126
  BorderStyle = bsDialog
  Caption = 'Processing settings'
  ClientHeight = 576
  ClientWidth = 424
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object grdEndOfCentralDirectory: TGroupBox
    Left = 8
    Top = 176
    Width = 201
    Height = 145
    Caption = 'End of central directory'
    TabOrder = 1
    object cbIgnoreEndOfCentralDirectory: TCheckBox
      Tag = 100
      Left = 8
      Top = 24
      Width = 169
      Height = 17
      Caption = 'Ignore end of central directory'
      TabOrder = 0
      OnClick = CheckBoxClick
    end
    object cbIgnoreDiskSplit: TCheckBox
      Tag = 101
      Left = 8
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Ignore disk split'
      TabOrder = 1
      OnClick = CheckBoxClick
    end
    object cbIgnoreNumberOfEntries: TCheckBox
      Tag = 102
      Left = 8
      Top = 72
      Width = 145
      Height = 17
      Caption = 'Ignore number of entries'
      TabOrder = 2
      OnClick = CheckBoxClick
    end
    object cbIgnoreCentralDirectoryOffset: TCheckBox
      Tag = 103
      Left = 8
      Top = 96
      Width = 169
      Height = 17
      Caption = 'Ignore central directory offset'
      TabOrder = 3
      OnClick = CheckBoxClick
    end
    object cbIgnoreComment: TCheckBox
      Tag = 104
      Left = 8
      Top = 120
      Width = 105
      Height = 17
      Caption = 'Ignore comment'
      TabOrder = 4
      OnClick = CheckBoxClick
    end
  end
  object grbCentralDirectoryHeaders: TGroupBox
    Left = 216
    Top = 176
    Width = 201
    Height = 361
    Caption = 'Central directory headers'
    TabOrder = 2
    object cbCDIgnoreCentralDirectory: TCheckBox
      Tag = 200
      Left = 8
      Top = 24
      Width = 137
      Height = 17
      Caption = 'Ignore central directory'
      TabOrder = 0
      OnClick = CheckBoxClick
    end
    object cbCDIgnoreSignature: TCheckBox
      Tag = 201
      Left = 8
      Top = 48
      Width = 137
      Height = 17
      Caption = 'Ignore header signature'
      TabOrder = 1
      OnClick = CheckBoxClick
    end
    object cbCDIgnoreVersions: TCheckBox
      Tag = 202
      Left = 8
      Top = 72
      Width = 97
      Height = 17
      Caption = 'Ignore versions'
      TabOrder = 2
      OnClick = CheckBoxClick
    end
    object cbCDClearEncryptionFlags: TCheckBox
      Tag = 203
      Left = 8
      Top = 96
      Width = 129
      Height = 17
      Caption = 'Clear encryption flags'
      TabOrder = 3
      OnClick = CheckBoxClick
    end
    object cbCDIgnoreCompressionMethod: TCheckBox
      Tag = 204
      Left = 8
      Top = 120
      Width = 161
      Height = 17
      Caption = 'Ignore compression method'
      TabOrder = 4
      OnClick = CheckBoxClick
    end
    object cbCDIgnoreModTime: TCheckBox
      Tag = 205
      Left = 8
      Top = 144
      Width = 121
      Height = 17
      Caption = 'Ignore last mod time'
      TabOrder = 5
      OnClick = CheckBoxClick
    end
    object cbCDIgnoreModDate: TCheckBox
      Tag = 206
      Left = 8
      Top = 168
      Width = 121
      Height = 17
      Caption = 'Ignore last mod date'
      TabOrder = 6
      OnClick = CheckBoxClick
    end
    object cbCDIgnoreCRC32: TCheckBox
      Tag = 207
      Left = 8
      Top = 192
      Width = 89
      Height = 17
      Caption = 'Ignore CRC32'
      TabOrder = 7
      OnClick = CheckBoxClick
    end
    object cbCDIgnoreSizes: TCheckBox
      Tag = 208
      Left = 8
      Top = 216
      Width = 81
      Height = 17
      Caption = 'Ignore sizes'
      TabOrder = 8
      OnClick = CheckBoxClick
    end
    object cbCDIgnoreInternalFileAttributes: TCheckBox
      Tag = 209
      Left = 8
      Top = 240
      Width = 161
      Height = 17
      Caption = 'Ignore internal file attributes'
      TabOrder = 9
      OnClick = CheckBoxClick
    end
    object cbCDIgnoreExternalFileAttributes: TCheckBox
      Tag = 210
      Left = 8
      Top = 264
      Width = 169
      Height = 17
      Caption = 'Ignore external file attributes'
      TabOrder = 10
      OnClick = CheckBoxClick
    end
    object cbCDIgnoreLocalHeaderOffset: TCheckBox
      Tag = 211
      Left = 8
      Top = 288
      Width = 145
      Height = 17
      Caption = 'Ignore local header offset'
      TabOrder = 11
      OnClick = CheckBoxClick
    end
    object cbCDIgnoreExtraField: TCheckBox
      Tag = 212
      Left = 8
      Top = 312
      Width = 105
      Height = 17
      Caption = 'Ignore extra field'
      TabOrder = 12
      OnClick = CheckBoxClick
    end
    object cbCDIgnoreFileComment: TCheckBox
      Tag = 213
      Left = 8
      Top = 336
      Width = 121
      Height = 17
      Caption = 'Ignore file comment'
      TabOrder = 13
      OnClick = CheckBoxClick
    end
  end
  object grbLocalHeaders: TGroupBox
    Left = 8
    Top = 328
    Width = 201
    Height = 241
    Caption = 'Local headers'
    TabOrder = 3
    object cbLHIgnoreSignature: TCheckBox
      Tag = 300
      Left = 8
      Top = 24
      Width = 137
      Height = 17
      Caption = 'Ignore header signature'
      TabOrder = 0
      OnClick = CheckBoxClick
    end
    object cbLHIgnoreVersions: TCheckBox
      Tag = 301
      Left = 8
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Ignore versions'
      TabOrder = 1
      OnClick = CheckBoxClick
    end
    object cbLHClearEncryptionFlags: TCheckBox
      Tag = 302
      Left = 8
      Top = 72
      Width = 129
      Height = 17
      Caption = 'Clear encryption flags'
      TabOrder = 2
      OnClick = CheckBoxClick
    end
    object cbLHIgnoreCompressionMethod: TCheckBox
      Tag = 303
      Left = 8
      Top = 96
      Width = 153
      Height = 17
      Caption = 'Ignore compression method'
      TabOrder = 3
      OnClick = CheckBoxClick
    end
    object cbLHIgnoreModTime: TCheckBox
      Tag = 304
      Left = 8
      Top = 120
      Width = 121
      Height = 17
      Caption = 'Ignore last mod time'
      TabOrder = 4
      OnClick = CheckBoxClick
    end
    object cbLHIgnoreModDate: TCheckBox
      Tag = 305
      Left = 8
      Top = 144
      Width = 121
      Height = 17
      Caption = 'Ignore last mod date'
      TabOrder = 5
      OnClick = CheckBoxClick
    end
    object cbLHIgnoreCRC32: TCheckBox
      Tag = 306
      Left = 8
      Top = 168
      Width = 89
      Height = 17
      Caption = 'Ignore CRC32'
      TabOrder = 6
      OnClick = CheckBoxClick
    end
    object cbLHIgnoreSizes: TCheckBox
      Tag = 307
      Left = 8
      Top = 192
      Width = 81
      Height = 17
      Caption = 'Ignore sizes'
      TabOrder = 7
      OnClick = CheckBoxClick
    end
    object cbLHIgnoreExtraField: TCheckBox
      Tag = 308
      Left = 8
      Top = 216
      Width = 113
      Height = 17
      Caption = 'Ignore extra field'
      TabOrder = 8
      OnClick = CheckBoxClick
    end
  end
  object btnAccept: TButton
    Left = 216
    Top = 544
    Width = 97
    Height = 25
    Caption = 'Accept'
    TabOrder = 4
    OnClick = btnAcceptClick
  end
  object btnClose: TButton
    Left = 320
    Top = 544
    Width = 97
    Height = 25
    Caption = 'Close'
    TabOrder = 5
    OnClick = btnCloseClick
  end
  object grbGeneral: TGroupBox
    Left = 8
    Top = 8
    Width = 409
    Height = 161
    Caption = 'General settings'
    TabOrder = 0
    object bvlGeneralhorSplit: TBevel
      Left = 8
      Top = 126
      Width = 393
      Height = 9
      Shape = bsTopLine
    end
    object vblGeneralHorSplit_File: TBevel
      Left = 8
      Top = 46
      Width = 393
      Height = 9
      Shape = bsTopLine
    end
    object lblFile: TLabel
      Left = 8
      Top = 24
      Width = 26
      Height = 13
      Caption = 'lblFile'
      Constraints.MaxWidth = 393
    end
    object cbIgnoreFileSignature: TCheckBox
      Left = 8
      Top = 136
      Width = 121
      Height = 17
      Caption = 'Ignore file signature'
      TabOrder = 4
      OnClick = CheckBoxClick
    end
    object cbAssumeCompressionMethods: TCheckBox
      Left = 144
      Top = 136
      Width = 169
      Height = 17
      Caption = 'Assume compression methods'
      TabOrder = 5
      OnClick = CheckBoxClick
    end
    object rbRebuild: TRadioButton
      Left = 8
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Rebuild archive'
      TabOrder = 0
      OnClick = RepairMethodClick
    end
    object rbExtract: TRadioButton
      Tag = 1
      Left = 112
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Extract archive'
      TabOrder = 1
      OnClick = RepairMethodClick
    end
    object lbleData: TLabeledEdit
      Left = 8
      Top = 96
      Width = 368
      Height = 21
      EditLabel.Width = 27
      EditLabel.Height = 13
      EditLabel.Caption = 'Data:'
      TabOrder = 2
      OnChange = lbleDataChange
    end
    object btnBrowse: TButton
      Left = 376
      Top = 96
      Width = 25
      Height = 21
      Caption = '...'
      TabOrder = 3
      OnClick = btnBrowseClick
    end
  end
  object diaSaveDialog: TSaveDialog
    DefaultExt = '.scs'
    Filter = 
      'SCS mod archive (*.scs)|*.scs|ZIP archive (*.zip)|*.zip|All file' +
      's (*.*)|*.*'
    Left = 384
  end
end
