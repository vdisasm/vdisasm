{
  *
  * OGDF mini wrapper
  *
}
unit ogdf;

interface

{$ALIGN ON}
{$MINENUMSIZE 4}


var
  // True if library is loaded and can be used.
  OGDFLoaded: boolean;

const
  LIB = 'grphlt.dll';

type
  TOGDFGraph = record
  end;

  TOGDFGraphAttr = record
  end;

  POGDFGraph = ^TOGDFGraph;
  POGDFGraphAttr = ^TOGDFGraphAttr;
  POGDFNode = type pointer;
  POGDFEdge = type pointer;
  DPolyline = type pointer;

  DPoint = record
    x, y: double;
  end;

  ListIterator_DPoint = type pointer;

type
  TGraphLayoutKind = (
    glk_fast_simple_hierarchy,
    glk_fast_hierarchy,
    glk_optimal_hierarchy
    );

type
  DPolyline_iterate_fn = procedure([ref] pt: DPoint; i: integer; ud: pointer); cdecl;

  // {$DEFINE OGDF_DELAYED}

var
  new_g: function: POGDFGraph; cdecl;                     // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};
  new_ga: function(g: POGDFGraph): POGDFGraphAttr; cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};
  graph_free: procedure(g: POGDFGraph; ga: POGDFGraphAttr); cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};
  graph_to_svg: procedure(ga: POGDFGraphAttr; filename: PAnsiChar); cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};

  node_new: function(g: POGDFGraph; ga: POGDFGraphAttr): POGDFNode; cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};

  node_get_x: procedure(ga: POGDFGraphAttr; n: POGDFNode; [ref] value: double); cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};
  node_get_y: procedure(ga: POGDFGraphAttr; n: POGDFNode; [ref] value: double); cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};
  node_get_w: procedure(ga: POGDFGraphAttr; n: POGDFNode; [ref] value: double); cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};
  node_get_h: procedure(ga: POGDFGraphAttr; n: POGDFNode; [ref] value: double); cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};

  node_set_x: procedure(ga: POGDFGraphAttr; n: POGDFNode; [ref] value: double); cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};
  node_set_y: procedure(ga: POGDFGraphAttr; n: POGDFNode; [ref] value: double); cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};
  node_set_w: procedure(ga: POGDFGraphAttr; n: POGDFNode; [ref] value: double); cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};
  node_set_h: procedure(ga: POGDFGraphAttr; n: POGDFNode; [ref] value: double); cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};

  edge_new: function(g: POGDFGraph; n0, n1: POGDFNode): POGDFEdge; cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};

  edge_bends: function(ga: POGDFGraphAttr; e: POGDFEdge): DPolyline; cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};

  layout: procedure(g: POGDFGraph; ga: POGDFGraphAttr; kind: TGraphLayoutKind; [ref] LayerDistance, NodeDistance: double); cdecl;
  // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};

  DPolyline_cnt: function(d: DPolyline): integer; cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};

  DPolyline_iterate: procedure(d: DPolyline; cb: DPolyline_iterate_fn; ud: pointer); cdecl; // external LIB {$IFDEF OGDF_DELAYED}delayed{$ENDIF};

implementation

uses
  System.SysUtils,
  WinApi.Windows;

var
  FModule: HMODULE;

procedure TryLoadLib;
var
  OldErrorMode: UINT;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    SetErrorMode(OldErrorMode or SEM_FAILCRITICALERRORS);

    FModule := LoadLibrary(LIB);

    if FModule <> 0 then
    begin
      @new_g := GetProcAddress(FModule, 'new_g');
      @new_ga := GetProcAddress(FModule, 'new_ga');
      @graph_free := GetProcAddress(FModule, 'graph_free');
      @graph_to_svg := GetProcAddress(FModule, 'graph_to_svg');
      @node_new := GetProcAddress(FModule, 'node_new');

      @node_get_x := GetProcAddress(FModule, 'node_get_x');
      @node_get_y := GetProcAddress(FModule, 'node_get_y');
      @node_get_w := GetProcAddress(FModule, 'node_get_w');
      @node_get_h := GetProcAddress(FModule, 'node_get_h');

      @node_set_x := GetProcAddress(FModule, 'node_set_x');
      @node_set_y := GetProcAddress(FModule, 'node_set_y');
      @node_set_w := GetProcAddress(FModule, 'node_set_w');
      @node_set_h := GetProcAddress(FModule, 'node_set_h');

      @edge_new := GetProcAddress(FModule, 'edge_new');
      @edge_bends := GetProcAddress(FModule, 'edge_bends');

      @layout := GetProcAddress(FModule, 'layout');

      @DPolyline_cnt := GetProcAddress(FModule, 'DPolyline_cnt');
      @DPolyline_iterate := GetProcAddress(FModule, 'DPolyline_iterate');

      OGDFLoaded := True;
    end;
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

initialization

TryLoadLib;

end.
