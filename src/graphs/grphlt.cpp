// grphlt.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"

#ifndef _WIN64
  #pragma comment(lib, "..\\..\\..\\ogdf\\Win32\\Release\\ogdf.lib")
  #pragma comment(lib, "..\\..\\..\\ogdf\\Win32\\Release\\coin.lib")
#else
  #pragma comment(lib, "..\\..\\..\\ogdf\\x64\\Release\\ogdf.lib")
  #pragma comment(lib, "..\\..\\..\\ogdf\\x64\\Release\\coin.lib")
#endif

#include <ogdf/layered/SugiyamaLayout.h>
#include <ogdf/layered/OptimalRanking.h>
#include <ogdf/layered/MedianHeuristic.h>
#include <ogdf/layered/FastSimpleHierarchyLayout.h>
#include <ogdf/layered/FastHierarchyLayout.h>
#include <ogdf/layered/OptimalHierarchyLayout.h>
#include <ogdf/fileformats/GraphIO.h>

using namespace ogdf;

//class TGraph {
//public:
//	Graph *G;
//	GraphAttributes *GA;
//};

//typedef TGraph * PGraph;
typedef node PNode;
typedef edge PEdge;

#define EXPORTS __declspec(dllexport)

extern "C" {

	Graph EXPORTS *new_g()
	{
		Graph *g = new Graph;
		return g;
	}

	GraphAttributes EXPORTS *new_ga(Graph *g)
	{
		GraphAttributes *ga = new GraphAttributes(*g);
		ga->setDirected(true);
		return ga;
	}

	//void EXPORTS graph_new(PGraph graph)
	//{
	//	graph->G = new Graph();

	//	graph->GA = new GraphAttributes();
	//	graph->GA->setDirected(true);
	//}

	void EXPORTS graph_free(Graph *g, GraphAttributes *ga)
	{
		delete g;
		delete ga;
	}

	void EXPORTS graph_to_svg(GraphAttributes *ga, char* filename)
	{
		GraphIO::drawSVG(*ga, filename);
	}


	PNode EXPORTS node_new(Graph *g, GraphAttributes* ga)
	{
		node node = g->newNode();
		//ga->x(node) = x;
		//ga->y(node) = y;
		//ga->width(node) = w;
		//ga->height(node) = h;
		return node;
	}

	//void EXPORTS node_coords(Graph *g, GraphAttributes *ga, PNode n, double **x, double **y, double **w, double **h)
	//{
	//	//if (x) *x = ga->x(n);
	//	//if (y) *y = ga->y(n);
	//	//if (w) *w = ga->width(n);
	//	//if (h) *h = ga->height(n);
	//	*x = &ga->x(n);
	//	*y = &ga->y(n);
	//	*w = &ga->width(n);
	//	*h = &ga->height(n);
	//}

	void EXPORTS node_get_x(GraphAttributes *ga, PNode n, double *value) { *value = ga->x(n); }
	void EXPORTS node_get_y(GraphAttributes *ga, PNode n, double *value) { *value = ga->y(n); }
	void EXPORTS node_get_w(GraphAttributes *ga, PNode n, double *value) { *value = ga->width(n); }
	void EXPORTS node_get_h(GraphAttributes *ga, PNode n, double *value) { *value = ga->height(n); }

	void EXPORTS node_set_x(GraphAttributes *ga, PNode n, double *value) { ga->x(n) = *value; }
	void EXPORTS node_set_y(GraphAttributes *ga, PNode n, double *value) { ga->y(n) = *value; }
	void EXPORTS node_set_w(GraphAttributes *ga, PNode n, double *value) { ga->width(n) = *value; }
	void EXPORTS node_set_h(GraphAttributes *ga, PNode n, double *value) { ga->height(n) = *value; }


	PEdge EXPORTS edge_new(Graph *g, PNode n0, PNode n1)
	{
		return g->newEdge(n0, n1);
	}

	DPolyline EXPORTS &edge_bends(GraphAttributes *ga, PEdge e)
	{
		return ga->bends(e);
	}

	enum TGraphLayoutKind {
		glk_fast_simple_hierarchy,
		glk_fast_hierarchy,
		glk_optimal_hierarchy,
	};

	void EXPORTS layout(Graph *g, GraphAttributes *ga, TGraphLayoutKind kind)
	{
		SugiyamaLayout SL;
		SL.setRanking(new OptimalRanking);
		SL.setCrossMin(new MedianHeuristic);

#define LAYER_DISTANCE 30.0
#define NODE_DISTANCE 25.0
#define WEIGHT_BALANCING 0.8

		switch (kind) {
		case glk_fast_simple_hierarchy: 
		{
			//firstcase:
			FastSimpleHierarchyLayout *fshl = new FastSimpleHierarchyLayout;
			fshl->layerDistance(LAYER_DISTANCE);
			fshl->nodeDistance(NODE_DISTANCE);
			SL.setLayout(fshl);
			break;
		}

		case glk_fast_hierarchy: 
		{
			FastHierarchyLayout *fhl = new FastHierarchyLayout;
			fhl->layerDistance(LAYER_DISTANCE);
			fhl->nodeDistance(NODE_DISTANCE);
			SL.setLayout(fhl);
			break;
		}
		case glk_optimal_hierarchy:
		{
			OptimalHierarchyLayout *ohl = new OptimalHierarchyLayout;
			ohl->layerDistance(LAYER_DISTANCE);
			ohl->nodeDistance(NODE_DISTANCE);
			ohl->weightBalancing(WEIGHT_BALANCING);
			SL.setLayout(ohl);
			break;
		}
		//default:
		//	goto firstcase;
		}

		SL.call(*ga);
	}

	int EXPORTS DPolyline_cnt(DPolyline &d)
	{
		return d.size();
	}

	void EXPORTS *DPolyline_iterate(DPolyline &d, void(cdecl* cb)(DPoint *pt, int i, void *ud), void *ud)
	{
		int i = 0;
		for (ListIterator<DPoint> it = d.begin(); it.valid(); it++)
		{
			cb(&(*it), i, ud);
			i++;
		}
		return 0;
	}

};
