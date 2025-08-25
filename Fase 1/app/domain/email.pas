unit Email;

{$mode ObjFPC}{$H+}

interface

type
  // Email state: Unread or Read
  TEmailState = (esUnread, esRead);

  PEmail = ^TEmail;

  TEmail = record
    Id: longint;
    Sender: ansistring;      // email address of the sender
    State: TEmailState;      // esUnread / esRead
    Scheduled: boolean;
    Subject: ansistring;
    Date: ansistring;
    MessageBody: ansistring;
  end;

var
  NextEmailId: longint = 1;

implementation

end.
