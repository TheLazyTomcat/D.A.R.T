object frmProcSettingsFrame_ZIP: TfrmProcSettingsFrame_ZIP
  Left = 0
  Top = 0
  Width = 561
  Height = 473
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object pnlBackground: TPanel
    Left = 0
    Top = 0
    Width = 561
    Height = 473
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object gbGeneral: TGroupBox
      Left = 0
      Top = 0
      Width = 561
      Height = 49
      Caption = 'General'
      TabOrder = 0
      OnMouseMove = GroupBoxMouseMove
      object cbAssumeCompressionMethod: TCheckBox
        Tag = 101
        Left = 8
        Top = 24
        Width = 161
        Height = 17
        Caption = 'Assume compression method'
        TabOrder = 0
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
    end
    object gbEndOfCentralDirectory: TGroupBox
      Left = 0
      Top = 56
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
      object cbEOCDIgnoreEndOfCentralDirectory: TCheckBox
        Tag = 201
        Left = 8
        Top = 24
        Width = 169
        Height = 17
        Caption = 'Ignore end of central directory'
        TabOrder = 0
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbEOCDIgnoreDiskSplit: TCheckBox
        Tag = 202
        Left = 192
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Ignore disk split'
        TabOrder = 1
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbEOCDIgnoreNumberOfEntries: TCheckBox
        Tag = 203
        Left = 376
        Top = 24
        Width = 145
        Height = 17
        Caption = 'Ignore number of entries'
        TabOrder = 2
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbEOCDIgnoreCentralDirectoryOffset: TCheckBox
        Tag = 204
        Left = 8
        Top = 48
        Width = 169
        Height = 17
        Caption = 'Ignore central directory offset'
        TabOrder = 3
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbEOCDIgnoreComment: TCheckBox
        Tag = 205
        Left = 192
        Top = 48
        Width = 97
        Height = 17
        Caption = 'Ignore comment'
        TabOrder = 4
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbEOCDLimitSearch: TCheckBox
        Tag = 206
        Left = 8
        Top = 80
        Width = 185
        Height = 17
        Caption = 'Limit search to one buffer (~1MiB)'
        TabOrder = 5
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
    end
    object gbCentralDirectoryHeaders: TGroupBox
      Left = 0
      Top = 168
      Width = 561
      Height = 145
      Caption = 'Central directory headers'
      TabOrder = 2
      OnMouseMove = GroupBoxMouseMove
      object cbCDIgnoreCentralDirectory: TCheckBox
        Tag = 301
        Left = 8
        Top = 24
        Width = 137
        Height = 17
        Caption = 'Ignore central directory'
        TabOrder = 0
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbCDIgnoreSignature: TCheckBox
        Tag = 302
        Left = 192
        Top = 24
        Width = 145
        Height = 17
        Caption = 'Ignore header signature'
        TabOrder = 1
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbCDIgnoreVersions: TCheckBox
        Tag = 303
        Left = 376
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Ignore versions'
        TabOrder = 2
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbCDClearEncryptionFlags: TCheckBox
        Tag = 304
        Left = 8
        Top = 48
        Width = 129
        Height = 17
        Caption = 'Clear encryption flags'
        TabOrder = 3
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbCDIgnoreCompressionMethod: TCheckBox
        Tag = 305
        Left = 192
        Top = 48
        Width = 161
        Height = 17
        Caption = 'Ignore compression method'
        TabOrder = 4
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbCDIgnoreModTime: TCheckBox
        Tag = 306
        Left = 376
        Top = 48
        Width = 161
        Height = 17
        Caption = 'Ignore last modification time'
        TabOrder = 5
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbCDIgnoreModDate: TCheckBox
        Tag = 307
        Left = 8
        Top = 72
        Width = 161
        Height = 17
        Caption = 'Ignore last modification date'
        TabOrder = 6
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbCDIgnoreCRC32: TCheckBox
        Tag = 308
        Left = 192
        Top = 72
        Width = 89
        Height = 17
        Caption = 'Ignore CRC32'
        TabOrder = 7
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbCDIgnoreSizes: TCheckBox
        Tag = 309
        Left = 376
        Top = 72
        Width = 81
        Height = 17
        Caption = 'Ignore sizes'
        TabOrder = 8
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbCDIgnoreInternalFileAttributes: TCheckBox
        Tag = 310
        Left = 8
        Top = 96
        Width = 161
        Height = 17
        Caption = 'Ignore internal file attributes'
        TabOrder = 9
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbCDIgnoreExternalFileAttributes: TCheckBox
        Tag = 311
        Left = 192
        Top = 96
        Width = 169
        Height = 17
        Caption = 'Ignore external file attributes'
        TabOrder = 10
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbCDIgnoreLocalHeaderOffset: TCheckBox
        Tag = 312
        Left = 376
        Top = 96
        Width = 153
        Height = 17
        Caption = 'Ignore local header offset'
        TabOrder = 11
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbCDIgnoreExtraField: TCheckBox
        Tag = 313
        Left = 8
        Top = 120
        Width = 113
        Height = 17
        Caption = 'Ignore extra field'
        TabOrder = 12
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbCDIgnoreFileComment: TCheckBox
        Tag = 314
        Left = 192
        Top = 120
        Width = 121
        Height = 17
        Caption = 'Ignore file comment'
        TabOrder = 13
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
    end
    object gbLocalHeaders: TGroupBox
      Left = 0
      Top = 320
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
        Tag = 402
        Left = 192
        Top = 24
        Width = 145
        Height = 17
        Caption = 'Ignore header signature'
        TabOrder = 0
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbLHIgnoreVersions: TCheckBox
        Tag = 403
        Left = 376
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Ignore versions'
        TabOrder = 1
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbLHClearEncryptionFlags: TCheckBox
        Tag = 404
        Left = 8
        Top = 48
        Width = 129
        Height = 17
        Caption = 'Clear encryption flags'
        TabOrder = 2
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbLHIgnoreCompressionMethod: TCheckBox
        Tag = 405
        Left = 192
        Top = 48
        Width = 161
        Height = 17
        Caption = 'Ignore compression method'
        TabOrder = 3
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbLHIgnoreModTime: TCheckBox
        Tag = 406
        Left = 376
        Top = 48
        Width = 121
        Height = 17
        Caption = 'Ignore last mod time'
        TabOrder = 4
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbLHIgnoreModDate: TCheckBox
        Tag = 407
        Left = 8
        Top = 72
        Width = 161
        Height = 17
        Caption = 'Ignore last modification date'
        TabOrder = 5
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbLHIgnoreCRC32: TCheckBox
        Tag = 408
        Left = 192
        Top = 72
        Width = 89
        Height = 17
        Caption = 'Ignore CRC32'
        TabOrder = 6
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbLHIgnoreSizes: TCheckBox
        Tag = 409
        Left = 376
        Top = 72
        Width = 81
        Height = 17
        Caption = 'Ignore sizes'
        TabOrder = 7
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbLHIgnoreExtraField: TCheckBox
        Tag = 411
        Left = 192
        Top = 96
        Width = 113
        Height = 17
        Caption = 'Ignore extra field'
        TabOrder = 8
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbLHIgnoreFileName: TCheckBox
        Tag = 410
        Left = 8
        Top = 96
        Width = 105
        Height = 17
        Caption = 'Ignore file name'
        TabOrder = 9
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbLHIgnoreLocalHeaders: TCheckBox
        Tag = 401
        Left = 8
        Top = 24
        Width = 121
        Height = 17
        Caption = 'Ignore local headers'
        TabOrder = 10
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
      object cbLHIgnoreDataDescriptor: TCheckBox
        Tag = 412
        Left = 8
        Top = 128
        Width = 129
        Height = 17
        Caption = 'Ignore data descriptor'
        TabOrder = 11
        OnClick = CheckBoxClick
        OnMouseMove = OptionMouseMove
      end
    end
  end
end
