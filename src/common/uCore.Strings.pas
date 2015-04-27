unit uCore.Strings;

interface


//
// GUI
//

resourcestring
  // Import/Export printed in disasm window.
  SNameImport = 'Import';
  SNameExport = 'Export';

  SCoreLibNotInitialized = 'Core library was not initialized';
  SCoreLibInitialized = 'Core library initialized';
  SInputNameForVA = 'Input Name for %x';
  SInputCommentForVA = 'Input Comment for %x';
  SEnterVA = 'Enter VA';
  SInputType = 'Input Type';
  SNoTypeLib = 'No type library';
  SWouldYouLikeToSaveChanges = 'Would you like to save changes?';

  SReadError = 'Read error';
  SWriteError = 'Write error';

  SSaveCurrentLayoutAsDefault = 'Save current layout as default?';

  SCantFindTextFmt = 'Can''t find "%s"'; // in list view
  SNothingFound = 'Nothing found';
  SFoundAtVA = 'Found at 0x%x';


  //
  // Core and other.
  //

resourcestring
  SVDisAsmVer = '1.8';
  SVDisAsmUrl = 'http://vdisasm.com/';
  SOnlineHelpUrl = 'http://vdisasm.com/vd/HTML/index.html';

  SVisitWebsite = 'Visit website';

  SNoLoadersFound = 'No loaders found';
  SSelectLoader = 'Select loader';
  SDbSaved = 'Database saved';
  SFailedToSaveDB = 'Failed to save database';
  SFailedToLoadCoreData = 'Failed to load core data';
  SFailedToSections = 'Failed to load sections';
  SFailedToLoadInputFileInfo = 'Failed to load input file info';
  SFailedToLoadTypMgrInfo = 'Failed to laod TypeMgr info';
  SNoDbPluginsToLoad = 'No db imported plugins to load';
  SLoadingDBPlugins = 'Loading db imported plugins';
  SFailedToLoadDBPlugins = 'Failed to load db plugins';

  SExtTypeLib = '.typelib';
  SExtDumpedText = '.txt';
  SExtDumpedExports = '.exports';
  SVddbDirPostfix = '_vddb';
  SVddbExt = '.vddb';

  SCursorNotPositioned = 'Cursor not positioned';

  SNewField = 'New Field'; // of structure
  SFieldName = 'Field Name';
  SFieldType = 'Field Type';

  SDbCleanupFailed = 'DB cleanup failed';
  SDbCleanupDone = 'DB cleanup done';

  SConfirmAction = 'Confirm action?';

  SModifyData = 'Modify data';
  STypeDoesntSupportEncoding = '"%s" type does not support encoding';
  SEncodingFailed = 'Encoding failed';

  SNamesCaption = 'Names';
  SExportsCaption = 'Exports';
  SImportsCaption = 'Imports';
  SRefsCaption = 'References';
  SSectionsCaption = 'Sections';
  SProblemsCaption = 'Problems';

  SDemoText = 'This is evaluation edition. Please buy this product.';

  SStartOfSSSSection = 'Start of "%s" section';

  // Export
  SPutExportSymbolForVA = 'Put export for 0x%x';

  // Debugger
  SDbgSessionAlreadyActive = 'Debug session is already active';

  // Analysis
  SNoBlockSpecified = 'No block specified';
  SAnalysisInProgress = 'Analysis in progress';

  // uFormFindBytes
  SStartSearchFromVA = 'from %x';

  SText = 'Text';
  SFind = 'Find';
  SFindText = 'Find text';
  SFindNext = 'Find next';
  SUpdate = 'Update';

  { String }
  SStringFoundAt = 'String found at 0x%x';
  SStringNotFoundStoppedAt = 'String not found. Stopped at 0x%x';
  SUnresolvedString = 'Unresolved string';

  { Types }
  SChoosePluginForType = 'Choose plugin for type "%s"';
  SFailedToWriteTypeAtVA = 'Failed to write type at 0x%x';

  { Copy special }

  SInvalidAddressString = 'Invalid address: %s';
  STextCopied = 'Text copied';

  { Frames reload }

  // SReloadingFrame = 'Reloading frame';

  { Debugger }
  SDebuggerIsInactive = 'Debugger is inactive';

  { Debugger\Watches }
  SWatches = 'Watches';
  SWatchExpr = 'Watch expression';
  SWatchDisabled = '<disabled>';

  { Evaluate }
  SBadExpression = 'Bad expression';
  SFailedToParseExpressionAtPositionD = 'Failed to parse expression at position %d';

implementation

end.
