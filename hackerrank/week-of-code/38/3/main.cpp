#include <climits>
#include <iostream>
#include <map>
#include <unordered_map>
#include <vector>

using namespace std;
int process(const int& interval, const int& time) {
  if ((time / interval) % 2 == 0) {
    return 0;
  } else {
    return interval - (time % interval);
  }
}
vector<int> dijkstra(const vector<vector<pair<int, int>>>& graph, const int& light_interval) {
  const int final_destination = graph.size() - 1;
  const int initial_source = 1;

  multimap<int, int> q;
  vector<int> distance(graph.size());
  vector<int> previous(graph.size());

  distance[initial_source] = 0;
  for (int vertex = 1; vertex <= final_destination; ++vertex) {
    if (vertex != initial_source) {
      distance[vertex] = INT_MAX;
    }
    previous[vertex] = 0;
    q.insert(pair<int, int>(distance[vertex], vertex));
  }

  while (!q.empty()) {
    auto it = q.begin();
    const int dist = (*it).first;
    const int u = (*it).second;
    q.erase(it);

    for (auto n : graph[u]) {
      const int neighbor = n.first;
      const int length = n.second;
      int alt = distance[u] + process(light_interval, distance[u]) + length;
      //cout << "u: " << u << ", neighbor: " << neighbor << endl;
      if (alt < 0) {continue;
              cout << "q:";
              for (auto k : q) {
			      cout << k.first << ", " << k.second << endl;
			      }
	      cout << "u: " << u << endl;
	      cout << "dist: " << dist << endl;
	      cout << "neighbor: " << neighbor << endl;
	      cout << "length: " << length << endl << endl;
      }
      
      if (alt < distance[neighbor]) {
        const int old_distance = distance[neighbor];
        distance[neighbor] = alt;
	previous[neighbor] = u;
	
	auto j = q.find(old_distance);
	while (j != q.end()) {
	  if (j->second == neighbor) {
            q.erase(j);
	    break;
          } else if (j->first != old_distance) {
            break;
          } else {
	  ++j;
          }
	}
	q.insert(pair<int, int>(distance[neighbor], neighbor));
      }
    }
  }

  return distance;
}

int main() {
  int n, k, m;
  cin >> n >> k >> m;
  
  vector<vector<pair<int, int>>> graph(n + 1);
  for (int counter = 0; counter < m; ++counter) {
    int i, j, t;
    cin >> i >> j >> t;

    if (i == j) {continue;}

    graph[i].emplace_back(j, t);
    graph[j].emplace_back(i, t);
  }
  auto answer = dijkstra(graph, k);
  for (int i = 0; i < answer.size(); ++i) {
    //cout << i << ": " << answer[i] << endl;
  }
  cout << dijkstra(graph, k)[n] << endl;

  return 0;
}
