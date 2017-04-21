unit TSLibHelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

	{ HTS_List }

  HTS_List = class helper for TList
  	function MaxIndex: Integer;
  end;

implementation

{ HTS_List }

function HTS_List.MaxIndex: Integer;
begin
 	Result := pred(Count);
end;

end.

