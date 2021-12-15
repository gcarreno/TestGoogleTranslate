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
{$IFDEF WINDOWS}
, Lmessages
{$ENDIF}
;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    aclMain: TActionList;
    actFileExit: TFileExit;
    actArraysTranslate: TAction;
    actObjectTranslate: TAction;
    btnArraysTranslate: TButton;
    btnObjectTranslate: TButton;
    cobArrayFrom: TComboBox;
    cobObjectFrom: TComboBox;
    cobArrayTo: TComboBox;
    cobObjectTo: TComboBox;
    grbWithArrays: TGroupBox;
    grbWithObject: TGroupBox;
    memArraysFrom: TMemo;
    memObjectFrom: TMemo;
    memArraysTo: TMemo;
    memObjectTo: TMemo;
    mnuObjectTranslate: TMenuItem;
    mnuTranslate: TMenuItem;
    mnuArraysTranslate: TMenuItem;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuMain: TMainMenu;
    panArrayButtons: TPanel;
    panObjectDetectedLanguage: TPanel;
    panObjectButtons: TPanel;
    panArrayDetectedLanguage: TPanel;
    pasArraysFromTo: TPairSplitter;
    pasObjectFromTo: TPairSplitter;
    pssArraysFrom: TPairSplitterSide;
    pssObjectFrom: TPairSplitterSide;
    pssArraysTo: TPairSplitterSide;
    pssObjectTo: TPairSplitterSide;
    procedure actArraysTranslateExecute(Sender: TObject);
    procedure actObjectTranslateExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure InitShortcuts;
    procedure FillCombos;

    function CallGoogleTranslate(AURL: String): TJSONStringType;
    function ShortCodetoLongCode(AShortCode: String): String;
  public
    {$IFDEF WINDOWS}
    // From Jamie at the Lazarus forum
    // Needed in Windows to make artifacts go away
    procedure WindowChangingPos(Var Msg:TLMWindowPosMsg); Message LM_WindowPOSChanging;
    {$ENDIF}
  end;

var
  frmMain: TfrmMain;

implementation

uses
  LCLType
;

const
  cArrayShortLanguages: Array [0..7] of String = (
    'auto',
    'en',
    'pt',
    'pl',
    'fr',
    'es',
    'it',
    'ru'
  );
  cArrayLongLanguages: Array [0..7] of String = (
    'Detect',
    'English',
    'Portuguese',
    'Polish',
    'French',
    'Spanish',
    'Italian',
    'Russian'
  );
  cJSONSentences = 'sentences';
  cJSONTranslation = 'trans';
  cJSONSource = 'src';
  cSourceLanguage = 'Source language: %s';

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

    cobObjectFrom.Items.Add(sItem);
    cobObjectTo.Items.Add(sItem);
  end;
  cobArrayFrom.ItemIndex:= 0;
  cobArrayTo.ItemIndex:= 1;

  cobObjectFrom.ItemIndex:= 0;
  cobObjectTo.ItemIndex:= 1;
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

function TfrmMain.ShortCodetoLongCode(AShortCode: String): String;
var
  Index: Integer;
begin
  Result:= 'Long name not found for ' + AShortCode;
  for Index:= 1 to High(cArrayShortLanguages) do
  begin
    if AShortCode = cArrayShortLanguages[Index] then
    begin
      Result:= cArrayLongLanguages[Index];
      break;
    end;
  end;
end;

{$IFDEF WINDOWS}
// From Jamie at the Lazarus forum
// Needed in Windows to make artifacts go away
procedure TfrmMain.WindowChangingPos(Var Msg:TLMWindowPosMsg);
 begin
  Inherited;
  grbWithArrays.Height:= ClientHeight div 2;
  pasArraysFromTo.Position:= ClientWidth div 2;
  pasObjectFromTo.Position:= ClientWidth div 2;
  Repaint;
 end;
{$ENDIF}

procedure TfrmMain.FormResize(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  Exit;
  {$ENDIF}
  grbWithArrays.Height:= ClientHeight div 2;
  pasArraysFromTo.Position:= ClientWidth div 2;
  pasObjectFromTo.Position:= ClientWidth div 2;
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
  memArraysTo.Clear;
  panArrayDetectedLanguage.Caption:= EmptyStr;
  Application.ProcessMessages;
  try

    if Length(memArraysFrom.Text) = 0 then
    begin
      ShowMessage('Need something to translate');
      exit;
    end;

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

    //ShowMessage(URL);

    strResponse:= CallGoogleTranslate(URL);
    try
      jdResponse:= GetJSON(strResponse);

      //memArraysTo.Append(jdResponse.FormatJSON); exit;

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
        if cobArrayFrom.ItemIndex = 0 then
        begin
           panArrayDetectedLanguage.Caption:= Format(cSourceLanguage, [
           ShortCodetoLongCode(jdResponse.FindPath('[2]').AsString)
          ]);
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

procedure TfrmMain.actObjectTranslateExecute(Sender: TObject);
var
  URL: String;
  Index: integer;
  strResponse: TJSONStringType;
  jdResponse: TJSONData;
  joTranslation, joSentence: TJSONObject;
  jaSentencesArray: TJSONArray;
begin
  actObjectTranslate.Enabled:= False;
  memObjectTo.Clear;
  panObjectDetectedLanguage.Caption:= EmptyStr;
  Application.ProcessMessages;
  try

    if Length(memObjectFrom.Text) = 0 then
    begin
      ShowMessage('Need something to translate');
      exit;
    end;

    if (cobObjectFrom.ItemIndex <= 0) and (cobObjectTo.ItemIndex <= 0) then
    begin
      ShowMessage('Cannot have language detection on both sides');
      exit;
    end;

    URL:='https://translate.googleapis.com/translate_a/single?client=gtx'
      +'&q='+HTTPEncode(memObjectFrom.Text)
      +'&sl='+cArrayShortLanguages[cobObjectFrom.ItemIndex]
      +'&tl='+cArrayShortLanguages[cobObjectTo.ItemIndex]
      +'&dt=t&dj=1'
      +'&ie=UTF-8&oe=UTF-8'
      ;

    //ShowMessage(URL);

    strResponse:= CallGoogleTranslate(URL);
    try
      jdResponse:= GetJSON(strResponse);

      //memObjectTo.Append(jdResponse.FormatJSON); exit;

      if (jdResponse <> nil) and (jdResponse.JSONType = jtObject) then
      begin
        joTranslation:= TJSONObject(jdResponse);
        jaSentencesArray:= TJSONArray(joTranslation.FindPath(cJSONSentences));
        for Index:=0 to Pred(jaSentencesArray.Count) do
        begin
          joSentence:= TJSONObject(jaSentencesArray[Index]);
          memObjectTo.Append(Trim(joSentence.Get(cJSONTranslation,'')));
        end;
        if cobObjectFrom.ItemIndex = 0 then
        begin
          panObjectDetectedLanguage.Caption:= Format(cSourceLanguage, [
            ShortCodetoLongCode(joTranslation.Get(cJSONSource,''))
          ]);
        end;
      end;
    finally
      jdResponse.Free;
    end;

  finally
    Application.ProcessMessages;
    actObjectTranslate.Enabled:= True;
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

