object frmProcSettingsSCS: TfrmProcSettingsSCS
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
    ParentBackground = True
    TabOrder = 0
    object gbEntries: TGroupBox
      Tag = 120
      Left = 0
      Top = 0
      Width = 561
      Height = 49
      Caption = 'Entries'
      TabOrder = 0
      OnMouseMove = GroupBoxMouseMove
      object cbIgnoreCRC32: TCheckBox
        Tag = 121
        Left = 8
        Top = 24
        Width = 89
        Height = 17
        Caption = 'Ignore CRC32'
        TabOrder = 0
        OnClick = CheckBoxClick
        OnMouseMove = SettingsMouseMove
      end
      object cbIgnoreCompressionFlag: TCheckBox
        Tag = 122
        Left = 192
        Top = 24
        Width = 137
        Height = 17
        Caption = 'Ignore compression flag'
        TabOrder = 1
        OnClick = CheckBoxClick
        OnMouseMove = SettingsMouseMove
      end
    end
    object gbPathResolve: TGroupBox
      Tag = 140
      Left = 0
      Top = 56
      Width = 561
      Height = 417
      Caption = 'Path resolve'
      TabOrder = 1
      OnMouseMove = GroupBoxMouseMove
      object lblHelpFiles: TLabel
        Left = 8
        Top = 208
        Width = 226
        Height = 13
        Caption = 'Help files (one file per line, full absolute paths):'
      end
      object lblCustomPaths: TLabel
        Left = 8
        Top = 48
        Width = 162
        Height = 13
        Caption = 'Custom paths (one path per line):'
      end
      object bvlHorSplit: TBevel
        Left = 8
        Top = 370
        Width = 545
        Height = 9
        Shape = bsTopLine
      end
      object lblLengthLimit: TLabel
        Left = 401
        Top = 386
        Width = 80
        Height = 13
        Alignment = taRightJustify
        Caption = 'Path length limit:'
      end
      object cbAssumeCityHash: TCheckBox
        Tag = 141
        Left = 8
        Top = 24
        Width = 113
        Height = 17
        Caption = 'Assume City hash'
        TabOrder = 0
        OnClick = CheckBoxClick
        OnMouseMove = SettingsMouseMove
      end
      object cbUsePredefinedPaths: TCheckBox
        Tag = 142
        Left = 192
        Top = 24
        Width = 129
        Height = 17
        Caption = 'Use predefined paths'
        TabOrder = 1
        OnClick = CheckBoxClick
        OnMouseMove = SettingsMouseMove
      end
      object cbExtractUnresolvedEntries: TCheckBox
        Tag = 143
        Left = 376
        Top = 24
        Width = 153
        Height = 17
        Caption = 'Extract unresolved entries'
        TabOrder = 2
        OnClick = CheckBoxClick
        OnMouseMove = SettingsMouseMove
      end
      object meHelpFiles: TMemo
        Tag = 145
        Left = 8
        Top = 224
        Width = 545
        Height = 137
        ScrollBars = ssBoth
        TabOrder = 4
        WordWrap = False
        OnMouseMove = SettingsMouseMove
      end
      object meCustomPaths: TMemo
        Tag = 144
        Left = 8
        Top = 64
        Width = 545
        Height = 137
        ScrollBars = ssBoth
        TabOrder = 3
        WordWrap = False
        OnMouseMove = SettingsMouseMove
      end
      object cbBruteForceResolve: TCheckBox
        Tag = 146
        Left = 8
        Top = 384
        Width = 121
        Height = 17
        Caption = 'Brute-force resolve'
        TabOrder = 5
        OnClick = CheckBoxClick
        OnMouseMove = SettingsMouseMove
      end
      object cbLimitedAlphabet: TCheckBox
        Tag = 147
        Left = 192
        Top = 384
        Width = 105
        Height = 17
        Caption = 'Limited alphabet'
        TabOrder = 6
        OnClick = CheckBoxClick
        OnMouseMove = SettingsMouseMove
      end
      object seLengthLimit: TSpinEdit
        Tag = 148
        Left = 488
        Top = 382
        Width = 65
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 7
        Value = 0
        OnMouseMove = SettingsMouseMove
      end
    end
  end
end
