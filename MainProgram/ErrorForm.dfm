object fErrorForm: TfErrorForm
  Left = 613
  Top = 432
  BorderStyle = bsDialog
  Caption = 'Error information'
  ClientHeight = 248
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object lblText: TLabel
    Left = 8
    Top = 64
    Width = 385
    Height = 57
    AutoSize = False
    Color = clBtnFace
    ParentColor = False
    WordWrap = True
  end
  object lblFileName: TLabel
    Left = 8
    Top = 8
    Width = 385
    Height = 13
    AutoSize = False
    Caption = 'lblFileName'
  end
  object bvlHorSplit: TBevel
    Left = 8
    Top = 56
    Width = 385
    Height = 9
    Shape = bsTopLine
  end
  object lblHint: TLabel
    Left = 25
    Top = 232
    Width = 368
    Height = 13
    Alignment = taRightJustify
    Caption = 
      'Press <Ctrl + C> to copy information displayed in this window in' +
      'to clipboard.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblFileSize: TLabel
    Left = 8
    Top = 32
    Width = 45
    Height = 13
    Caption = 'lblFileSize'
  end
  object grbTechnical: TGroupBox
    Left = 8
    Top = 128
    Width = 385
    Height = 97
    Caption = 'Technical information'
    TabOrder = 0
    object lblExceptionClass_l: TLabel
      Left = 136
      Top = 72
      Width = 89
      Height = 13
      Alignment = taRightJustify
      Caption = 'Exception class:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblObject_l: TLabel
      Left = 25
      Top = 24
      Width = 40
      Height = 13
      Alignment = taRightJustify
      Caption = 'Object:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblMethod_l: TLabel
      Left = 19
      Top = 48
      Width = 46
      Height = 13
      Alignment = taRightJustify
      Caption = 'Method:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblThread_l: TLabel
      Left = 6
      Top = 72
      Width = 59
      Height = 13
      Alignment = taRightJustify
      Caption = 'Thread ID:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblObject: TLabel
      Left = 72
      Top = 24
      Width = 3
      Height = 13
    end
    object lblMethod: TLabel
      Left = 72
      Top = 48
      Width = 3
      Height = 13
    end
    object lblThread: TLabel
      Left = 72
      Top = 72
      Width = 3
      Height = 13
    end
    object lblExceptionClass: TLabel
      Left = 232
      Top = 72
      Width = 3
      Height = 13
    end
  end
end
