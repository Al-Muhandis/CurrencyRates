unit cbrvalutes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl
  ;

type

  TValuteCurrency = (vcUnknown, vcRUB, vcUSD, vcEUR, vcGBP, vcKZT, vcTRY, vcJPY, vcCNY, vcTJS, vcUZS, vcUAH, vcKGS,
    vcHKD);

  { TValuteItem }

  TValuteItem = class
  private
    FCharCode: String;
    FID: String;
    FName: String;
    FNominal: Word;
    FNumCode: Word;
    FPrevious: Double;
    FValue: Double;
  published
    property ID: String read FID write FID;
    property NumCode: Word read FNumCode write FNumCode;
    property CharCode: String read FCharCode write FCharCode;
    property Nominal: Word read FNominal write FNominal;
    property Name: String read FName write FName;
    property Value: Double read FValue write FValue;
    property Previous: Double read FPrevious write FPrevious;
  end;

  TValuteItemList = specialize TFPGObjectList<TValuteItem>;

  { TValuteList }

  TValuteList = class
  private
    FValuteItems: TValuteItemList;
    function GetRUB: TValuteItem;
    function GetValutes(Index: TValuteCurrency): TValuteItem;
  public
    constructor Create;
    destructor Destroy; override;
    property Valutes[Index: TValuteCurrency]: TValuteItem read GetValutes; default;
    property RUB: TValuteItem read GetRUB;
  published
    property USD: TValuteItem index vcUSD read GetValutes;
    property EUR: TValuteItem index vcEUR read GetValutes;
    property GBP: TValuteItem index vcGBP read GetValutes;
    property KZT: TValuteItem index vcKZT read GetValutes;
    property &TRY: TValuteItem index vcTRY read GetValutes;
    property JPY: TValuteItem index vcJPY read GetValutes;
    property CNY: TValuteItem index vcCNY read GetValutes;
    property TJS: TValuteItem index vcTJS read GetValutes;
    property UZS: TValuteItem index vcUZS read GetValutes;
    property UAH: TValuteItem index vcUAH read GetValutes;
    property KGS: TValuteItem index vcKGS read GetValutes;
    property HKD: TValuteItem index vcHKD read GetValutes;
  end;

  { TDailyValutes }

  TDailyValutes = class
  private
    FDate: String;
    FPreviousDate: String;
    FPreviousURL: String;
    FTimeStamp: String;
    FValute: TValuteList;
    FLastUpdate: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;
    function IsRelevant: Boolean;
    procedure CheckRelevance;
    procedure UpdateData;
    function CalcRate(InCurrency, OutCurrency: TValuteCurrency): Double;
  published
    property Date: String read FDate write FDate;
    property PreviousDate: String read FPreviousDate write FPreviousDate;
    property PreviousURL: String read FPreviousURL write FPreviousURL;
    property Timestamp: String read FTimeStamp write FTimestamp;
    property Valute: TValuteList read FValute write FValute;
  end;

var
  _CBRDailyValutes: TDailyValutes;
  CriticalSectionCBR: TRTLCriticalSection;

function StrToValuteCurrency(const aValute: String): TValuteCurrency;

implementation

uses
  DateUtils, fphttpclient, fpjsonrtti, jsonparser, jsonscanner
  ;

//const
//  CharCodes: array [TValuteCurrency] of PChar = ('', 'RUB', 'USD', 'EUR', 'GBP', 'KZT', 'TRY', 'JPY', 'CNY', 'TJS', 'UZS', 'UAH', 'KGS');

function StrToValuteCurrency(const aValute: String): TValuteCurrency;
begin                        //  (vcUnknown, vcRUB, vcUSD, vcEUR, vcGBP, vcKZT, vcTRY, vcJPY, vcCNY, vcTJS, vcUZS, vcUAH);
  case AnsiLowerCase(aValute) of
    'rub', 'руб', 'р', '₽', 'rubles', 'ruble', 'рублей', 'рубль', 'рубля', 'рубли': Result:=vcRUB;
    'usd', 'долларов', '$', 'dollars', 'dollar', 'доллар':                          Result:=vcUSD;
    'eur', 'euro', 'евро', '€':                                                     Result:=vcEUR;
    'gbp', 'фунт', '£':                                                             Result:=vcGBP;
    'kzt', 'тенге', '₸':                                                            Result:=vcKZT;
    'try', 'лира', 'лир':                                                           Result:=vcTRY;
    'jpy', 'иена', '¥', 'иен':                                                      Result:=vcJPY;
    'cny', 'юань', 'юаней', 'rmb', 'юани':                                          Result:=vcCNY;
    'tjs', 'сомони':                                                                Result:=vcTJS;
    'uzs', 'сум':                                                                   Result:=vcUZS;
    'uah', 'гривны', 'гривна', 'гривен', '₴':                                       Result:=vcUAH;
    'kgs', 'сом', 'сомов', 'сома':                                                  Result:=vcKGS;
    'hkd':                                                                          Result:=vcHKD;
  else
    Result:=vcUnknown;
  end;
end;

{ TValuteList }

function TValuteList.GetValutes(Index: TValuteCurrency): TValuteItem;
var
  aIndex: Integer;
begin
  if Index=vcRUB then
    Exit(GetRUB);
  aIndex:=Ord(Index);
  if not Assigned(FValuteItems[aIndex]) then
    FValuteItems[aIndex]:=TValuteItem.Create;
  Result:=FValuteItems[aIndex];
end;

function TValuteList.GetRUB: TValuteItem;
begin
  if not Assigned(FValuteItems[Ord(vcRUB)]) then
  begin
    FValuteItems[Ord(vcRUB)]:=TValuteItem.Create;
    with FValuteItems[Ord(vcRUB)] do
    begin
      ID:='R00000';    // Какой правильный ID для рубля?
      NumCode:=643;
      CharCode:='RUB';
      Nominal:=1;
      Name:='Российский рубль';
      Value:=1;
      Previous:=1;
    end;
  end;
  Result:=FValuteItems[Ord(vcRUB)];
end;

constructor TValuteList.Create;
begin
  FValuteItems:=TValuteItemList.Create(True);
  FValuteItems.Count:=ord(High(TValuteCurrency)) + 1;
end;

destructor TValuteList.Destroy;
begin
  FValuteItems.Free;
  inherited Destroy;
end;

{ TDailyValutes }

constructor TDailyValutes.Create;
begin
  FValute:=TValuteList.Create;
  FLastUpdate:=Now-2;
  InitCriticalSection(CriticalSectionCBR);
end;

destructor TDailyValutes.Destroy;
begin
  DoneCriticalSection(CriticalSectionCBR);
  FValute.Free;
  inherited Destroy;
end;

function TDailyValutes.IsRelevant: Boolean;
begin
  Result:=Now-FLastUpdate<1/6;  // Меньше 4 часов
end;

procedure TDailyValutes.CheckRelevance;
begin
  if not IsRelevant then
    UpdateData;
end;

procedure TDailyValutes.UpdateData;
var
  DeStreamer: TJSONDeStreamer;
  aJSON: String;
const
  CBR_ENDPOINT='https://www.cbr-xml-daily.ru/daily_json.js';
begin
  try
    aJSON:=TFPHTTPClient.SimpleGet(CBR_ENDPOINT);
  except
    aJSON:=EmptyStr;        // to-do
  end;
  if aJSON=EmptyStr then
    Exit;
  DeStreamer := TJSONDeStreamer.Create(nil);
  EnterCriticalSection(CriticalSectionCBR);
  try
    try
      DeStreamer.JSONToObject(aJSON, self);
      FLastUpdate:=Now;
    except
      // to-do
    end;
  finally 
    LeaveCriticalSection(CriticalSectionCBR);
    DeStreamer.Destroy;
  end;
end;

function TDailyValutes.CalcRate(InCurrency, OutCurrency: TValuteCurrency): Double;
var
  aValIn, aValOut: TValuteItem;
  aRubleValue: Double;
begin
  aValOut:=Valute.Valutes[OutCurrency];
  aValIn :=Valute.Valutes[InCurrency];
  aRubleValue:=aValIn.Value/aValIn.Nominal;
  Result:=aRubleValue/aValOut.Value*aValOut.Nominal;
end;

initialization
  _CBRDailyValutes:=TDailyValutes.Create;

finalization
  _CBRDailyValutes.Free;

end.

