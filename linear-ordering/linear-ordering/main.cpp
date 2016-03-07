#include <iostream>
#include <fstream>
#include <string>
#include <list>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>
#include <iterator>
#include <streambuf>
#include <cmath>
#include <numeric>

#include <float.h>

using namespace std;

vector<string> results;

vector<string> split(const string& s, const string& delim, const bool keep_empty = true)
{
	vector<string> result;
	if (delim.empty()) {
		result.push_back(s);
		return result;
	}
	string::const_iterator substart = s.begin(), subend;

	for (;;) {
		subend = search(substart, s.end(), delim.begin(), delim.end());
		string temp(substart, subend);
		if (keep_empty || !temp.empty()) {
			result.push_back(temp);
		}
		if (subend == s.end()) {
			break;
		}
		substart = subend + delim.size();
	}
	return result;
}

vector<string> readFile(const string& file_path)
{
	ifstream is(file_path);
	vector<string> result;

	string current_node;
	while (is >> current_node)
		result.push_back(current_node);

	return result;
}

struct Node
{
	string value;
	vector<Node*> edges;
	bool marked;
	bool semi_marked;

	Node(const string& s) : value(s), edges(vector<Node*>()), marked(false), semi_marked(false) {}
	bool Node::operator==(const Node& rhs) const
	{
		return this->value == rhs.value;
	}
};



bool any_Unmarked(const vector<Node*>& g)
{
	for (Node* n : g) {
		if (n->marked == false)
			return true;
	}

	return false;
}

Node* pick_First_Unmarked(vector<Node*>& g)
{
	for (Node* n : g) {
		if (n->marked == false)
			return n;
	}

	return new Node("");
}

bool visit(Node* n)
{
	if (n->semi_marked) {
		return true;
	}

	else if (n->marked == false) {
		n->semi_marked = true;
		for (Node* v : n->edges) {
			if(visit(v))
				return true;
		}

		n->marked = true;
		n->semi_marked = false;
		results.insert(results.begin(), n->value);
	}

	return false;
}

bool topological_Sort(vector<Node*>& g)
{
	while (any_Unmarked(g)) {
		Node* n = pick_First_Unmarked(g);
		if(visit(n))
			return true;
	}

	return false;
}

bool elem(vector<Node*>& nodes, const string& s)
{

	for (Node* n : nodes) {
		if (n->value == s) {
			return true;
		}
	}

	return false;
}

Node* get(vector<Node*>& nodes, const string& s)
{

	for (Node* n : nodes) {
		if (*n == s) {
			return n;
		}
	}

	return nullptr;
}

void make_Graph(vector<string>& nodes)
{
	vector<Node*> vertices;
	for (const string& s: nodes) {
		if (elem(vertices, s) == false) {
			vertices.push_back(new Node(s));
		}
	}

	for (int i = 0; i < nodes.size(); i += 2) {
		for (Node* n : vertices) {
			if (n->value == nodes[i]) {
				Node* temp = get(vertices, nodes[i + 1]);
				n->edges.push_back(temp);
			}
		}
	}

	if (topological_Sort(vertices)) {
		cerr << "The graph is not a directed acyclic graph. There's no linear ordering.\n";
	}
}

int main(int argc, char *argv[])
{
	string input1 = "P -> Q\nL && M -> P\nB && L -> M\nA && P -> L\nA && B -> L\nA\nB\n"; //example input
	string file = argv[1];

	//feed knowledge base from file
	vector<string> nodes = readFile(file);

	make_Graph(nodes);

	for (const string& s : results) {
		if (s != results.back())
			cout << s << " => ";
		else
			cout << s << endl;
	}

	//printProblem(words);
	char c;
	cin >> c;

	return 0;
}