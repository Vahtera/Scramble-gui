unit Scramble;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit;

type
  TS = class(TForm)
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuSettings: TMenuItem;
    MenuExit: TMenuItem;
    MenuPlayers: TMenuItem;
    MenuRounds: TMenuItem;
    MenuItem6: TMenuItem;
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
    procedure MenuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonRollClick(Sender: TObject);
    procedure ButtonSubmitClick(Sender: TObject);
    procedure ButtonHintClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  S: TS;
  Words: TStringList;
  Language: string;
  CurrentPlayer, CurrentRound, NumPlayers, NumRounds, Turns, Points, i, j, k, MaxPoints, Winner: Integer;
  PlayerScores: array of Integer;
  Answer, Scrambled, InputWord: string;
  LangText: array[0..1] of string = ('English', 'Finnish');


implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

procedure LoadWords(FileName: string);
begin
  Words := TStringList.Create;
  try
    Words.LoadFromFile(FileName);
  except
    on E: Exception do
    begin
      Writeln('Error loading file: ', E.Message);
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

  Randomize;
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

procedure EndGame();
begin
  S.LabelPoints.Text := 'Points: -';
  S.LabelTurns.Text := 'Turn - / -';
  S.LabelPlayer.Text := 'Player: -';
  S.LabelWordDisplay.Text := 'Press Start to play.';
  S.EditAnswer.Text := '';
  S.ButtonRoll.Enabled := True;
  S.ButtonSubmit.Enabled := False;
  S.ButtonHint.Enabled := False;
end;

procedure ShowPoints();
begin
  S.LabelPoints.Text := 'Points: ' + IntToStr(Points);
end;

procedure ShowTurns();
begin
    S.LabelTurns.Text := 'Turn: ' + IntToStr(CurrentRound) + ' / ' + IntToStr(Turns);
end;

procedure ShowPlayers();
begin
  S.LabelPlayer.Text := 'Player: ' + IntToStr(CurrentPlayer);
end;

procedure DisplayWord();
begin
  Randomize;
  S.LabelWordDisplay.Text := '';
  Answer := Words[Random(Words.Count)];
  Scrambled := ShuffleWord(Answer);

  S.LabelWordDisplay.Text := AnsiUpperCase(AddSpaces(Scrambled));
//  S.EditAnswer.Text := Answer;
  Points := Length(Answer);
end;

procedure RoundOver();
begin
    Points := 0;
    CurrentRound := 1;
    CurrentPlayer := CurrentPlayer + 1;
    ShowPlayers;
    if CurrentPlayer > NumPlayers then
    begin
      EndGame;
    end
    else
    begin
      ShowMessage('Player ' + IntToStr(CurrentPlayer) + ' turn!');
      CurrentRound := 1;
      S.EditAnswer.Text := '';
      DisplayWord;
      ShowPlayers;
      ShowTurns;
      ShowPoints;
    end;
end;

procedure StartRound();
var
  players, I: Integer;
begin
  players := NumPlayers - 1;
  SetLength(PlayerScores, NumPlayers);

  for I := 0 to players do
  begin
    PlayerScores[I] := 0;
  end;

  CurrentRound := 1;
  CurrentPlayer := 1;
  S.ButtonRoll.Enabled := False;
  S.ButtonSubmit.Enabled := True;
  S.ButtonHint.Enabled := True;
  ShowTurns;
  DisplayWord;
  ShowPoints;
  ShowPlayers;
end;


procedure TS.ButtonHintClick(Sender: TObject);
var
  hint: string;
begin
  hint := Copy(Answer, 1, CurrentRound);
  if CurrentRound = Turns then
  begin
    S.LabelInfo.Text := 'The correct word was: ' + Answer;
    Points := 0;
    ShowPoints;
    ShowTurns;
    RoundOver();
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
begin
  S.LabelInfo.Text := 'Player 1 turn.';
  StartRound();
end;

procedure TS.ButtonSubmitClick(Sender: TObject);
var
  ans: boolean;
begin
  ans := CheckAnswer(S.EditAnswer.Text);
  if CurrentRound = Turns then
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
    end;
    CurrentRound := CurrentRound + 1;
    ShowTurns;
  end;
end;

procedure TS.FormCreate(Sender: TObject);
begin
  Randomize;
  Language := 'English';
  Turns := 3;
  Points := 5;
  NumPlayers := 2;
  LoadWords(Language + '.lst');
  S.EditAnswer.Text := '';
  S.ButtonSubmit.Enabled := False;
  S.ButtonHint.Enabled := False;
  S.ClientWidth := 640;
  S.ClientHeight := 190;
//  S.StatusBar1.
end;

procedure TS.MenuExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.
