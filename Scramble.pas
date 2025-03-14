unit Scramble;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Objects, System.IOUtils,
  FMX.StdActns, System.Actions, FMX.ActnList, System.SyncObjs;

type
  TS = class(TForm)
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuSettings: TMenuItem;
    MenuExit: TMenuItem;
    MenuPlayers: TMenuItem;
    MenuRounds: TMenuItem;
    LabelWordDisplay: TLabel;
    ButtonRoll: TButton;
    EditAnswer: TEdit;
    ButtonSubmit: TButton;
    ButtonHint: TButton;
    LabelPoints: TLabel;
    LabelTurns: TLabel;
    Panel1: TPanel;
    LabelPlayer: TLabel;
    LabelInfo: TLabel;
    Rectangle1: TRectangle;
    LabelRound: TLabel;
    MenuPlayer1: TMenuItem;
    MenuPlayer2: TMenuItem;
    MenuPlayer3: TMenuItem;
    MenuPlayer4: TMenuItem;
    MenuPlayer5: TMenuItem;
    MenuRound3: TMenuItem;
    MenuRound2: TMenuItem;
    MenuRound1: TMenuItem;
    MenuRound5: TMenuItem;
    MenuRound4: TMenuItem;
    ActionList1: TActionList;
    FileExit1: TFileExit;
    FileHideApp1: TFileHideApp;
    FileHideAppOthers1: TFileHideAppOthers;
    mnuApple: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuAbout: TMenuItem;
    PanelAbout: TPanel;
    LabelTitle: TLabel;
    ButtonAboutOK: TButton;
    LabelAboutText: TLabel;
    procedure MenuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonRollClick(Sender: TObject);
    procedure ButtonSubmitClick(Sender: TObject);
    procedure ButtonHintClick(Sender: TObject);
    procedure MenuPlayer1Click(Sender: TObject);
    procedure MenuPlayer2Click(Sender: TObject);
    procedure MenuPlayer3Click(Sender: TObject);
    procedure MenuPlayer4Click(Sender: TObject);
    procedure MenuPlayer5Click(Sender: TObject);
    procedure MenuRound4Click(Sender: TObject);
    procedure MenuRound1Click(Sender: TObject);
    procedure MenuRound2Click(Sender: TObject);
    procedure MenuRound3Click(Sender: TObject);
    procedure MenuRound5Click(Sender: TObject);
    procedure ButtonAboutOKClick(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  S: TS;
  Words: TStringList;
  Language, AppPath, ResourcePath: string;
  CurrentPlayer, CurrentRound, CurrentTurn, NumPlayers, NumRounds, Turns, Points, i, j, k, MaxPoints, Winner: Integer;
  PlayerScores: array of Integer;
  Answer, Scrambled, InputWord: string;
  LangText: array[0..1] of string = ('English', 'Finnish');
  BGColor: TAlphaColor;


implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}
{$R *.Macintosh.fmx MACOS}

{$include CommitInfo}

procedure LoadWords(FileName: string);
{ Load words from file }
begin
  Words := TStringList.Create;
  try
    Words.LoadFromFile(FileName);
  except
    on E: Exception do
    begin
      ShowMessage('Error loading file: ' + E.Message);
      Application.Terminate;
    end;
  end;
end;

function AddSpaces(const S: string): string;
{ Add spaces between characters in a string }
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    Result := Result + S[I] + ' ';
  Result := Trim(Result); // Remove trailing space
end;

function CheckAnswer(const S: string): boolean;
{ Check if answer is correct }
begin
  if AnsiLowerCase(S) = AnsiLowerCase(Answer) then
    Result := True
  else
    Result := False
end;

function ShuffleWord(const Word: string): string;
{ Scramble the letters in a word}
var
  Chars: array of Char;
  i, r: Integer;
  Temp: Char;
begin
  SetLength(Chars, Length(Word));
  for i := 1 to Length(Word) do
    Chars[i - 1] := Word[i];

  for i := High(Chars) downto 1 do
  begin
    r := Random(i + 1); // generate a random index between 0 and i
    Temp := Chars[i];
    Chars[i] := Chars[r];
    Chars[r] := Temp;
  end;

  SetLength(Result, Length(Chars));
  for i := 0 to High(Chars) do
    Result[i + 1] := Chars[i];

  // or use SetString as suggested by Uli Gerhardt
  // SetString(Result, PChar(@Chars[0]), Length(Chars));
end;

procedure ShowPoints();
{ Update point display }
begin
  S.LabelPoints.Text := 'Points: ' + IntToStr(Points);
end;

procedure ShowTurns();
{ Update Turn display }
begin
    S.LabelTurns.Text := 'Turn: ' + IntToStr(CurrentRound) + ' / ' + IntToStr(Turns);
    S.LabelRound.Text := 'Round: ' + IntToStr(CurrentTurn)+ ' / ' + IntToStr(NumRounds);
end;

procedure ShowPlayers();
{ Update Player display }
begin
  S.LabelPlayer.Text := 'Player: ' + IntToStr(CurrentPlayer)+ ' / ' + IntToStr(NumPlayers);
end;

procedure DisplayWord();
{ Randomize and display the word }
begin
  S.LabelWordDisplay.Text := '';
  Answer := Words[Random(Words.Count)];
  Scrambled := ShuffleWord(Answer);

  S.LabelWordDisplay.Text := AnsiUpperCase(AddSpaces(Scrambled));
  Points := Length(Answer);
end;

procedure StartRound();
{ Start a round of the game }
begin
  CurrentRound := 1;
  CurrentPlayer := 1;
  S.ButtonRoll.Enabled := False;
  S.ButtonSubmit.Enabled := True;
  S.ButtonHint.Enabled := True;
  S.EditAnswer.Text := '';
  ShowTurns;
  DisplayWord;
  ShowPoints;
  ShowPlayers;
end;

procedure EndGame();
{ End game round, and check if game over }
var
  message: string;
  I: Integer;
begin
  if CurrentTurn = NumRounds then
  begin
    message := 'Final scores:' + sLineBreak + sLineBreak;
    for I := 1 to NumPlayers do
      begin
        message := message + 'Player ' + IntToStr(I) + ': ' + IntToStr(PlayerScores[I-1]) + sLineBreak;
      end;
    ShowMessage(message);
    S.LabelPoints.Text := 'Points: -';
    S.LabelTurns.Text := 'Turn - / -';
    S.LabelPlayer.Text := 'Player: - / -';
    S.LabelRound.Text := 'Round: - / -';
    S.LabelWordDisplay.Text := 'Press Start to play.';
    S.EditAnswer.Text := '';
    S.ButtonRoll.Enabled := True;
    S.ButtonSubmit.Enabled := False;
    S.ButtonHint.Enabled := False;
    S.MenuSettings.Enabled := True;
    CurrentRound := 1;
    CurrentPlayer := 1;
    CurrentTurn := 0;
  end
  else
  begin
    CurrentTurn := CurrentTurn + 1;
    ShowMessage('Round ' + IntToStr(CurrentTurn) + ' of ' + IntToStr(NumRounds));
    StartRound;
  end;
end;

procedure RoundOver();
{ End round check if new round starts, or game over }
begin
    Points := 0;
    CurrentRound := 1;
    CurrentPlayer := CurrentPlayer + 1;
    if CurrentPlayer > NumPlayers then
    begin
      EndGame;
    end
    else
    begin
      ShowPlayers;
      ShowMessage('Player ' + IntToStr(CurrentPlayer) + ' turn!');
      CurrentRound := 1;
      S.EditAnswer.Text := '';
      DisplayWord;
      ShowPlayers;
      ShowTurns;
      ShowPoints;
    end;
end;


procedure StartTurn();
{ Start a new Turn }
begin
  if CurrentTurn > NumRounds then
  EndGame
  else
  //CurrentTurn := CurrentTurn + 1;
  StartRound();
end;

function IsFinalTurn(): boolean;
{ Check if last turn }
begin
  Result := CurrentRound = NumRounds;
end;

procedure TS.ButtonAboutOKClick(Sender: TObject);
begin
  S.PanelAbout.Visible := False;
end;

procedure TS.ButtonHintClick(Sender: TObject);
{ Request a hint, and adjust score }
var
  hint: string;
begin
  hint := Copy(Answer, 1, CurrentRound);
  if CurrentRound > NumRounds then
  begin
    S.LabelInfo.Text := 'The correct word was: ' + Answer;
    Points := 0;
    ShowPoints;
    ShowTurns;
    RoundOver;
  end
  else
  begin
    Points := Points - 1;
    CurrentRound := CurrentRound + 1;
    S.LabelInfo.Text := 'The correct word starts with: ' + hint;
    S.EditAnswer.Text := hint;
    ShowPoints;
    ShowTurns;
  end;
end;

procedure TS.ButtonRollClick(Sender: TObject);
{ Start the game }
var
  players, I: Integer;
begin
  S.MenuSettings.Enabled := False;
  players := NumPlayers - 1;
  SetLength(PlayerScores, NumPlayers);

  for I := 0 to players do
  begin
    PlayerScores[I] := 0;
  end;

  S.LabelInfo.Text := 'Player 1 turn.';
  CurrentTurn := 1;
  StartTurn();
end;

procedure TS.ButtonSubmitClick(Sender: TObject);
{ Submit answer }
var
  ans: boolean;
begin
  S.Rectangle1.Fill.Color := BGColor;
  ans := CheckAnswer(Trim(S.EditAnswer.Text));
  if CurrentRound > Turns then
  begin
    ShowPoints;
    S.LabelInfo.Text := 'Round Over! Sorry! The correct word was: ' + Answer;
    RoundOver;
  end
  else
  begin
    if ans then
    begin
      S.LabelInfo.Text := 'Correct! The answer was: ' + Answer + '. You got ' + IntToStr(Points) + ' points';
      PlayerScores[CurrentPlayer - 1] := PlayerScores[CurrentPlayer - 1] + Points;
      RoundOver;
    end
    else
    begin
      S.LabelInfo.Text := 'Incorrect.';
      S.Rectangle1.Fill.Color := TAlphaColors.Red;
      CurrentRound := CurrentRound + 1;
      if CurrentRound > Turns then
      begin
        S.LabelInfo.Text := 'Incorrect. The correct word was: ' + Answer;
        RoundOver;
      end;
      ShowTurns;
    end;
  end;
end;

procedure TS.FormCreate(Sender: TObject);
var
  ListFile, FName: string;
begin

  AppPath := ExtractFilePath(ParamStr(0)); // get application path
  Randomize;

  { Initialize some values }
  S.PanelAbout.Visible := False;
  S.PanelAbout.Align := TAlignLayout.Contents;
  Language := 'English';
  Turns := 3;
  Points := 5;
  NumPlayers := 2;
  NumRounds := 2;

  { Settings for running on a macOS machine. }
  {$IFDEF MACOS}
    SetLength(AppPath, Length(AppPath) - 6); // To get rid of 'MacOS/' folder where the binary is located.
    AppPath := TPath.Combine(AppPath, 'Resources/StartUp/'); // Add the correct path to resources.
  {$ENDIF}

  { Show/Hide menuitems based on OS. }
  mnuApple.Visible := (TOSVersion.Platform = pfMacOS);
  MenuExit.Visible := (TOSVersion.Platform <> pfMacOS);

  { Get Filename from Language, and then load the corresponding wordlist }
  FName := AnsiLowerCase(Language) + '.lst';
  ListFile := TPath.Combine(AppPath, FName);
  LoadWords(ListFile);

  { Set app default state }
  BGColor := S.Rectangle1.Fill.Color;
  S.EditAnswer.Text := '';
  S.ButtonSubmit.Enabled := False;
  S.ButtonHint.Enabled := False;
  S.ClientWidth := 640;
  S.ClientHeight := 190;
  S.LabelWordDisplay.Text := 'Scramble';
  ShowPlayers;
  ShowTurns;
  ShowPoints;
end;

procedure TS.MenuAboutClick(Sender: TObject);
var
 pad: string;
begin
  pad := '    ';
  S.LabelTitle.Position.Y := 0;
  S.LabelTitle.Position.X := S.PanelAbout.Width - S.LabelTitle.Width;
  S.PanelAbout.Visible := True;
  S.LabelAboutText.Text := sLineBreak +
  pad + 'Version: ' + IntToStr(GetVersion) + sLineBreak +
  pad + 'Copyright © 2025 Anna Vahtera' + sLineBreak +
  pad + 'Licensed under AGPL-3.0' + sLineBreak + sLineBreak +
  pad + 'A word guessing game. You are presented with a scrambled version of a word.' + sLineBreak +
  pad + 'Your job is to guess which word it is.' + sLineBreak +
  pad + 'Points are awarded based on the length of the word. Using hints will reduce points.'
  ;
end;

procedure TS.MenuExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TS.MenuRound1Click(Sender: TObject);
begin
  NumRounds := 1;
  ShowTurns;
end;

procedure TS.MenuRound2Click(Sender: TObject);
begin
  NumRounds := 2;
  ShowTurns;
end;

procedure TS.MenuRound3Click(Sender: TObject);
begin
  NumRounds := 3;
  ShowTurns;
end;

procedure TS.MenuRound4Click(Sender: TObject);
begin
  NumRounds := 4;
  ShowTurns;
end;

procedure TS.MenuRound5Click(Sender: TObject);
begin
  NumRounds := 5;
  ShowTurns;
end;

procedure TS.MenuPlayer1Click(Sender: TObject);
begin
  NumPlayers := 1;
  ShowPlayers;
end;

procedure TS.MenuPlayer2Click(Sender: TObject);
begin
  NumPlayers := 2;
  ShowPlayers;
end;

procedure TS.MenuPlayer3Click(Sender: TObject);
begin
  NumPlayers := 3;
  ShowPlayers;
end;

procedure TS.MenuPlayer4Click(Sender: TObject);
begin
  NumPlayers := 4;
  ShowPlayers;
end;

procedure TS.MenuPlayer5Click(Sender: TObject);
begin
  NumPlayers := 5;
  ShowPlayers;
end;

end.
