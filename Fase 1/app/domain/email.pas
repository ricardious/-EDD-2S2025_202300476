unit Email;

{$mode ObjFPC}{$H+}

interface

type
  // Email state: Unread or Read
  TEmailState = (esUnread, esRead);

  PEmail = ^TEmail;
  TEmail = record
    Id: LongInt;
    Sender: AnsiString;      // email address of the sender
    State: TEmailState;      // esUnread / esRead
    Scheduled: Boolean;
    Subject: AnsiString;
    Date: AnsiString;
    MessageBody: AnsiString;
  end;

implementation

end.

