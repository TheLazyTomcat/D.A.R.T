object fResultInfoForm: TfResultInfoForm
  Left = 761
  Top = 398
  BorderStyle = bsDialog
  Caption = 'Result information'
  ClientHeight = 272
  ClientWidth = 496
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object lblResultCaption: TLabel
    Left = 8
    Top = 8
    Width = 92
    Height = 13
    Caption = 'lblResultCaption'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblHint: TLabel
    Left = 191
    Top = 254
    Width = 298
    Height = 13
    Alignment = taRightJustify
    Caption = 'Press <Ctrl + C> to copy displayed information into clipboard.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object meResultInfoText: TMemo
    Left = 8
    Top = 32
    Width = 481
    Height = 217
    TabStop = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
end
