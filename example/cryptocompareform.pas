unit CryptoCompareForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, IniPropStorage
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
  _CryptoCompare.APYKey:=EdtToken.Text;
  EdtRate.Text:=FloatToStr(_CryptoCompare.CryptoCompare[CmbBxIn.Text, CmbBxOut.Text]);
end;

end.

