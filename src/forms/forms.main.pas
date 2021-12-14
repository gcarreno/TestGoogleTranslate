unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, Forms
, Controls
, Graphics
, Dialogs
, ActnList
, Menus
, StdActns
, StdCtrls
, PairSplitter
, ExtCtrls
, fphttpclient
, fpjson
, jsonparser
, opensslsockets
, HTTPDefs
;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    aclMain: TActionList;
    actFileExit: TFileExit;
    actArraysTranslate: TAction;
    btnArraysTranslate: TButton;
    cobArrayFrom: TComboBox;
    cobArrayTo: TComboBox;
    grbWithArrays: TGroupBox;
    memArraysFrom: TMemo;
    memArraysTo: TMemo;
    mnuTranslate: TMenuItem;
    mnuArraysTranslate: TMenuItem;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuMain: TMainMenu;
    panArrayButtons: TPanel;
    pasArraysFromTo: TPairSplitter;
    pssArraysFrom: TPairSplitterSide;
    pssArraysTo: TPairSplitterSide;
    procedure actArraysTranslateExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure InitShortcuts;
    procedure FillCombos;

    function CallGoogleTranslate(AURL: String): TJSONStringType;
  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
  LCLType
;

const
  cArrayShortLanguages: Array [0..6] of String = (
    'auto',
    'en',
    'pt',
    'pl',
    'fr',
    'es',
    'it'
  );
  cArrayLongLanguages: Array [0..6] of String = (
    'Detect',
    'English',
    'Portuguese',
    'Polish',
    'French',
    'Spanish',
    'Italian'
  );

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.InitShortcuts;
begin
{$IFDEF LINUX}
  actFileExit.ShortCut := KeyToShortCut(VK_Q, [ssCtrl]);
{$ENDIF}
{$IFDEF WINDOWS}
  actFileExit.ShortCut := KeyToShortCut(VK_X, [ssAlt]);
{$ENDIF}
end;

procedure TfrmMain.FillCombos;
var
  sItem: String;
begin
  for sItem in cArrayLongLanguages do
  begin
    cobArrayFrom.Items.Add(sItem);
    cobArrayTo.Items.Add(sItem);
  end;
  cobArrayFrom.ItemIndex:= 0;
  cobArrayTo.ItemIndex:= 1;
end;

function TfrmMain.CallGoogleTranslate(AURL: String): TJSONStringType;
var
  client: TFPHTTPClient;
  doc: TStringList;
begin
  Result:= EmptyStr;
  doc:=TStringList.Create;
  client:=TFPHTTPClient.Create(nil);
  try
    client.Get(AURL,doc);
    Result:=doc.Text;
  finally
    doc.Free;
    client.Free;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:= True;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfrmMain.actArraysTranslateExecute(Sender: TObject);
var
  URL: String;
  Index: integer;
  strResponse: TJSONStringType;
  jdResponse, jdTranslation, jdTranslationArray: TJSONData;
  jaTranslation, jaTranslationArray: TJSONArray;
begin
  actArraysTranslate.Enabled:= False;
  Application.ProcessMessages;
  memArraysTo.Clear;
  try

    if (cobArrayFrom.ItemIndex <= 0) and (cobArrayTo.ItemIndex <= 0) then
    begin
      ShowMessage('Cannot have language detection on both sides');
      exit;
    end;

    URL:='https://translate.googleapis.com/translate_a/single?client=gtx'
      +'&q='+HTTPEncode(memArraysFrom.Text)
      +'&sl='+cArrayShortLanguages[cobArrayFrom.ItemIndex]
      +'&tl='+cArrayShortLanguages[cobArrayTo.ItemIndex]
      +'&dt=t'
      +'&ie=UTF-8&oe=UTF-8'
      ;
    strResponse:= CallGoogleTranslate(URL);
    try
      jdResponse:= GetJSON(strResponse);
      jdTranslation:= jdResponse.FindPath('[0]');
      if (jdTranslation <> nil) and (jdTranslation.JSONType = jtArray) then
      begin
        jaTranslation:= TJSONArray(jdTranslation);
        for index:= 0 to Pred(jaTranslation.Count) do
        begin
          jdTranslationArray:= jaTranslation[Index];
          if (jdTranslationArray <> nil) and (jdTranslationArray.JSONType = jtArray) then
          begin
            jaTranslationArray:= TJSONArray(jdTranslationArray);
            memArraysTo.Append(Trim(jaTranslationArray[0].AsString));
          end;
        end;
      end;
    finally
      jdResponse.Free;
    end;

  finally
    Application.ProcessMessages;
    actArraysTranslate.Enabled:= True;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitShortcuts;
  FillCombos;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  //
end;

end.

