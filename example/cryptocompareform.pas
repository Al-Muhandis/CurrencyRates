unit CryptoCompareForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, IniPropStorage, ComCtrls
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnGet: TButton;
    CmbBxIn: TComboBox;
    CmbBxOut: TComboBox;
    EdtToken: TLabeledEdit;
    IniPrpStrg: TIniPropStorage;
    EdtRate: TLabeledEdit;
    LblCurrencyIn: TLabel;
    LblCurrencyOut: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure BtnGetClick({%H-}Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  cryptocompare
  ;

{ TForm1 }

procedure TForm1.BtnGetClick(Sender: TObject);
begin
  _CryptoCompare.APIKey:=EdtToken.Text;
  EdtRate.Text:=FloatToStr(_CryptoCompare.CryptoCompare[CmbBxIn.Text, CmbBxOut.Text]);
  Memo1.Text:=_CryptoCompare.JSONReply.FormatJSON();
end;

end.

