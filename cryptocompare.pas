unit cryptocompare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson
  ;

type

  { TCryptoCompare }

  TCryptoCompare = class
  private
    FAPIKey: String;
    FCriticalSection: TRTLCriticalSection;
    FJSON: TJSONObject;
    FLastUpdate: TDateTime;
    function GetCryptoCompare(aCodeIn: String; aCodeOut: String): Double;
    function GetJSONReply: TJSONObject;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function IsRelevant: Boolean; 
    procedure CheckRelevance;
    procedure UpdateData;
    property APIKey: String read FAPIKey write FAPIKey;
    property CryptoCompare[aCodeIn: String; aCodeOut: String]: Double read GetCryptoCompare;
    property JSONReply: TJSONObject read GetJSONReply;
  end;

var
  _CryptoCompare: TCryptoCompare;

implementation

uses
  DateUtils, fphttpclient, jsonparser, jsonscanner, opensslsockets
  ;

const
  CC_ENDPOINT='https://min-api.cryptocompare.com/data/pricemulti?fsyms=BTC,ETH,PASC,XRP,BCH,LTC,TON,ETC&tsyms=USD,EUR,RUB&apikey=%s';

{ TCryptoCompare }

function TCryptoCompare.GetJSONReply: TJSONObject;
begin
  CheckRelevance;
  Result:=FJSON;
end;

constructor TCryptoCompare.Create;
begin
  InitCriticalSection(FCriticalSection);
end;

function TCryptoCompare.GetCryptoCompare(aCodeIn: String; aCodeOut: String): Double;
var
  aCC: TJSONObject;
  aRate: QWord;
const
  CCODE='USD';
begin
  Result:=0;
  aCodeIn:=UpperCase(aCodeIn);
  aCodeOut:=UpperCase(aCodeOut);
  if SameStr(aCodeIn, aCodeOut) then
    Exit(1);
  if JSONReply.Find(aCodeIn, aCC) then
  begin
    Result:=aCC.Get(aCodeOut, 0);
    if Result=0 then
    begin
      aRate:=aCC.Get(CCODE, 0);
      if JSONReply.Find(aCodeOut, aCC) then
      begin
        Result:=aCC.Get(CCODE, 0);
        if Result<>0 then
          Result:=aRate/Result;
      end;
    end;
    Exit;
  end;
  if JSONReply.Find(aCodeOut, aCC) then
  begin
    Result:=aCC.Get(aCodeIn, 0);
    if Result<>0 then
      Result:=1/Result;
  end;
end;

destructor TCryptoCompare.Destroy;
begin
  DoneCriticalSection(FCriticalSection);
  FJSON.Free;
  inherited Destroy;
end;

function TCryptoCompare.IsRelevant: Boolean;
begin
  Result:=Now-FLastUpdate<1/24/2;  // Меньше 1/2 часа
end;

procedure TCryptoCompare.CheckRelevance;
begin
  if not IsRelevant then
    UpdateData;
end;

procedure TCryptoCompare.UpdateData;
var
  S: String;
begin
  if FAPIKey.IsEmpty then
    raise Exception.Create('CryptoCompare API: no token specified!');
  try
    S:=TFPHTTPClient.SimpleGet(Format(CC_ENDPOINT, [FApiKey]));
  except
    S:=EmptyStr;        // to-do
  end;
  if S=EmptyStr then
    Exit;
  EnterCriticalSection(FCriticalSection);
  try
    FJSON.Free;
    try
      FJSON:=GetJSON(S) as TJSONObject;
    except
      // to-do
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
  FLastUpdate:=Now;
end;

initialization
  _CryptoCompare:=TCryptoCompare.Create;

finalization
  _CryptoCompare.Free;

end.

