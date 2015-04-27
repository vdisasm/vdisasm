unit uLoader;

interface

uses
  System.SysUtils, // supports

  uCore,
  uPlugins, // TVDPluginManager

  VDAPI;

// Returns False if loading was cancelled.
function RunLoader(
  const c: TVDCore;
  const LoaderFileName: string;
  LoaderFormatId: integer;
  out Entry: TVA
  ): boolean;

implementation

uses
  uLoaderTask,
  uLoaderForm;

procedure ApplyEntry(const c: TVDCore; const Task: TVDLoaderTask);
var
  VA: TVA;
  bMakeEntry: boolean;
begin
  VA := Task.Entry;

  bMakeEntry := True;

  if (VA = BAD_VA) or (not c.VM.Exists(VA)) then
  begin
    bMakeEntry := False;
    if not c.VM.GetFirstVA(@VA) then
      exit;
  end;

  if bMakeEntry then
    c.ExportSymbols.Put(VA, PROGRAM_ENTRY_NAME, 0);

  c.ChangeVA(VA, True);
end;

procedure ApplyLoaderTask(const c: TVDCore; const Task: TVDLoaderTask);
var
  CoreData: PVDCoreData;
  Sec: TSecBase;
  SecFromFile: TSecFromFile;
  a: AnsiString;
begin
  CoreData := c.GetData;

  // Cpu name
  a := AnsiString(Task.CpuName);
  if Length(a) + 1 > Length(CoreData.CodeType) then
    SetLength(a, Length(CoreData.CodeType) - 1);
  Move(PAnsiChar(a)^, CoreData.CodeType[0], Length(a));
  CoreData.CodeType[Length(a)] := #0;

  // Endianness
  CoreData.Endianness := Task.Endianness;

  // AddressSize
  CoreData.AddressSize := Task.AddressSize;

  // Sections
  for Sec in Task.SecList do
  begin
    // Section from file.
    if Sec is TSecFromFile then
    begin
      SecFromFile := Sec as TSecFromFile;
      if SecFromFile.Mapped then
        c.VM.Sections.AddFromMappedFile(SecFromFile.FileName, SecFromFile.Name,
          SecFromFile.Offset, SecFromFile.RawSize, SecFromFile.VA, SecFromFile.Flags)
      else
        c.VM.Sections.AddFromFile(SecFromFile.FileName, SecFromFile.Name,
          SecFromFile.Offset, SecFromFile.RawSize, SecFromFile.VirtSize,
          SecFromFile.VA, SecFromFile.Flags);
    end
    else
      raise Exception.Create('Unhandled section type');
  end;

  // Entry
  ApplyEntry(c, Task);
end;

function RunLoader(
  const c: TVDCore;
  const LoaderFileName: string;
  LoaderFormatId: integer;
  out Entry: TVA
  ): boolean;
var
  pm: TVDPluginManager;
  Task: IVDLoaderTask;
  TaskClass: TVDLoaderTask;
  applied: boolean;
  tmpentry: TVA;
begin
  applied := False;
  tmpentry := BAD_VA;

  pm := (c.GetPluginManager as TVDPluginManager);
  pm.LoadPluginInternalAndInit(LoaderFileName, 0,
    procedure(const Info: TLoadedPluginInfo)
    var
      ldr: IVDLoaderPlugin;
    begin
      if Supports(Info.CreatedPlugin, IVDLoaderPlugin) then
      begin
        ldr := Info.CreatedPlugin as IVDLoaderPlugin;

        // Fill task.
        Task := TVDLoaderTask.Create;
        TaskClass := Task as TVDLoaderTask;

        ldr.FillTask(Task, LoaderFormatId);

        // ---------------------------------------------------------------------
        // Some processing on task.
        TaskClass.SortSectionsByVa;
        FindSectionConflicts(TaskClass);

        // Default code is "X32"
        if TaskClass.CpuName = '' then
          TaskClass.CpuName := TCpuName.X32;

        // Default address size is 32 bits.
        if not(TaskClass.AddressSize in [2, 4, 8]) then
          TaskClass.AddressSize := 4;
        // ---------------------------------------------------------------------

        // Display and edit + validate.
        if DisplayLoaderTask(TaskClass) then
        begin
          // Apply task.
          ApplyLoaderTask(c, TaskClass);

          ldr.TaskApplied;

          applied := True;
          tmpentry := TaskClass.Entry;
        end;
      end;
    end);

  result := applied;
  Entry := tmpentry;
end;

end.
