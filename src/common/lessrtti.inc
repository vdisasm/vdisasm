{ Reduce EXE size by disabling as much of RTTI as possible (delphi 2009/2010) }
{$IF CompilerVersion >= 21}
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}
