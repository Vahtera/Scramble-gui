unit Scramble;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Objects, System.IOUtils,
  FMX.StdActns, System.Actions, FMX.ActnList;

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


implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}
{$R *.Macintosh.fmx MACOS}

procedure LoadWords(FileName: string);
//r
//test: string;
begin
  Words := TStringList.Create;
//test := System.SysUtils.ExpandFileName(FileName);
//ShowMessage(test);
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
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    Result := Result + S[I] + ' ';
  Result := Trim(Result); // Remove trailing space
end;

function CheckAnswer(const S: string): boolean;
begin
  if AnsiLowerCase(S) = AnsiLowerCase(Answer) then
    Result := True
  else
    Result := False
end;

function ShuffleWord(const Word: string): string;
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
begin
  S.LabelPoints.Text := 'Points: ' + IntToStr(Points);
end;

procedure ShowTurns();
begin
    S.LabelTurns.Text := 'Turn: ' + IntToStr(CurrentRound) + ' / ' + IntToStr(Turns);
    S.LabelRound.Text := 'Round: ' + IntToStr(CurrentTurn)+ ' / ' + IntToStr(NumRounds);
end;

procedure ShowPlayers();
begin
  S.LabelPlayer.Text := 'Player: ' + IntToStr(CurrentPlayer)+ ' / ' + IntToStr(NumPlayers);
end;

procedure DisplayWord();
begin
  S.LabelWordDisplay.Text := '';
  Answer := Words[Random(Words.Count)];
  Scrambled := ShuffleWord(Answer);

  S.LabelWordDisplay.Text := AnsiUpperCase(AddSpaces(Scrambled));
  Points := Length(Answer);
end;

procedure StartRound();
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
begin
  if CurrentTurn > NumRounds then
  EndGame
  else
  //CurrentTurn := CurrentTurn + 1;
  StartRound();
end;

function IsFinalTurn(): boolean;
begin
  Result := CurrentRound = NumRounds;
end;

procedure TS.ButtonHintClick(Sender: TObject);
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
var
  ans: boolean;
begin
  ans := CheckAnswer(S.EditAnswer.Text);
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
  ListFile: string;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  Randomize;
  //ShowMessage(AppPath);
  Language := 'English';
  Turns := 3;
  Points := 5;
  NumPlayers := 2;
  NumRounds := 2;

  { Settings for running on a macOS machine. }
  {$IFDEF MACOS}
    SetLength(AppPath, Length(AppPath) - 6); // To get rid of 'MacOS/' folder where the binary is located.
    AppPath := AppPath + 'Resources/StartUp/'; // Add the correct path to resources.
  {$ENDIF}

  { Show/Hide menuitems based on OS. }
  mnuApple.Visible := (TOSVersion.Platform = pfMacOS);
  MenuExit.Visible := (TOSVersion.Platform <> pfMacOS);

  ListFile := AppPath + AnsiLowerCase(Language) + '.lst';
  LoadWords(ListFile);
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
