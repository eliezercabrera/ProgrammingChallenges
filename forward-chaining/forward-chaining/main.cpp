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

	string current_word;
	while (is >> current_word) {
		transform(current_word.begin(), current_word.end(), current_word.begin(), ::tolower);
		result.push_back(current_word);
	}

	return result;
}

double B(double q)
{
	if (q >= 1)
		return 0;
	if (q == 0)
		return 1;
	return -(q*log2(q) + (1 - q)*log2(1 - q));
}

double P(char)
{
	return 0;
}

double Remainder(const int A, const vector<string>& words, const int p_total, const int n_total)
{
	double result = 0;

	vector<vector<string>> E(26);
	vector<int> p(26);
	vector<int> n(26);

	for (char i = 0; i < 26; ++i) {
		for (const string& s : words) {
			//cerr << (char)('a' + i) << ' ';
			if (s.length() >= A && s[A-1] == (char)('a' + i))
				E[i].push_back(s);
		}
	}

	for (int i = 0; i < 26; ++i) {
		int counter = 0;
		for (const string& s : E[i]) {
			if (s.length() % 2 == 1) {
				counter++;
			}
		}
		p[i] = counter;
		n[i] = E[i].size() - p[i];

		//cerr << E[i].size() << endl;
	}

	for (int k = 0; k < E.size(); ++k) {
		const double place_holder = p[k] + n[k];
		if (place_holder == 0) {
			continue;
		}
		const double b_arg = (double)(p[k]) / (place_holder);
		const double last = B(b_arg);
		result += (double)(p[k] + n[k]) / (p_total + n_total) * last;
	}
	
	return result;
}

double Gain(const int A, const vector<string>& words)
{
	int counter = 0;

	for (const string& s : words) {
		if (s.length() % 2 == 1) {
			counter++;
		}
	}

	const int p_total = counter;
	const int n_total = words.size() - p_total;
	const double div = (double)p_total / (p_total + n_total);
	const double what = B(div);

	return what - Remainder(A, words, p_total, n_total);
}

void printProblem(const vector<string>& words)
{
	for (int i: { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }) {
		cout << i << ":\n" << Gain(i, words) << '\n' << endl;
	}
}

int main(int argc, char *argv[])
{
	string input1 = "P -> Q\nL && M -> P\nB && L -> M\nA && P -> L\nA && B -> L\nA\nB\n"; //example input
	string file = argv[1];

	//feed knowledge base from file
	vector<string> words = readFile(file);

	printProblem(words);

	return 0;
}