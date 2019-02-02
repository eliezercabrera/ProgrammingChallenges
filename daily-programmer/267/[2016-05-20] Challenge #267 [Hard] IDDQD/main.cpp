#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

struct Point
{
  int x;
  int y;

  Point(const int _x, const int _y) : x(_x), y(_y) {}
  Point() : x(-1), y(-1) {}
};

bool operator==(const Point& lhs, const Point& rhs)
{
  return lhs.x == rhs.x && lhs.y == rhs.y;
}

istream& operator>>(istream& is, Point& p)
{
  is >> p.y;
  is >> p.x;

  return is;
}

struct {
  bool operator()(const Point& a, const Point& b)
  {
    if (a.y == b.y)
    {
      return a.x < b.x;
    }
    return a.y < b.y;
  }
} horizontal_Compare;

struct {
  bool operator()(const Point& a, const Point& b)
  {
    if (a.x == b.x)
    {
      return a.y < b.y;
    }
    return a.x < b.x;
  }
} vertical_Compare;

int diagonal_Index(const Point& p)
{
  return p.x - p.y;
}

struct {
  bool operator()(const Point& a, const Point& b)
  {
    const int ia = diagonal_Index(a);
    const int ib = diagonal_Index(b);

    if (ia == ib)
    {
      return a.x < b.x;
    }
    return ia < ib;
  }
} diagonal_Compare;

int anti_Diagonal_Index(const Point& p)
{
  return p.x + p.y;
}

struct {
  bool operator()(const Point& a, const Point& b)
  {
    const int aia = anti_Diagonal_Index(a);
    const int aib = anti_Diagonal_Index(b);

    if (aia == aib)
    {
      return a.x < b.x;
    }
    return aia < aib;
  }
} anti_Diagonal_Compare;

void populate_Point_Vectors(const int height, const int width,
  vector<Point>& h, vector<Point>& v,
  vector<Point>& d, vector<Point>& a)
{
  Point current_point;

  while (cin >> current_point)
  {
    h.push_back(current_point);
    h.emplace_back(-1, current_point.y - 1);
    h.emplace_back(-1, current_point.y);
    h.emplace_back(-1, current_point.y + 1);

    v.push_back(current_point);
    v.emplace_back(current_point.x - 1, -1);
    v.emplace_back(current_point.x, -1);
    v.emplace_back(current_point.x + 1, -1);

    d.push_back(current_point);
    if (current_point.x == current_point.y)
    {
      d.emplace_back(-1, 0);
      d.emplace_back(-1, -1);
      d.emplace_back(0, -1);
    }
    else if (current_point.x < current_point.y)
    {
      const int y = current_point.y - current_point.x - 1;
      d.emplace_back(-1, y - 1);
      d.emplace_back(-1, y);
      d.emplace_back(-1, y + 1);
    }
    else
    {
      const int x = current_point.x - current_point.y - 1;
      d.emplace_back(x - 1, -1);
      d.emplace_back(x, -1);
      d.emplace_back(x + 1, -1);
    }

    a.push_back(current_point);
    if (current_point.x + current_point.y == width - 1)
    {
      a.emplace_back(width - 1, -1);
      a.emplace_back(width, -1);
      a.emplace_back(width    , 0 );
    }
    else if (current_point.y + current_point.x < width - 1)
    {
      const int x = current_point.x + current_point.y + 1;
      a.emplace_back(x - 1, -1);
      a.emplace_back(x, -1);
      a.emplace_back(x + 1, -1);
    }
    else
    {
      const int y = current_point.y + current_point.x - width;
      a.emplace_back(width, y - 1);
      a.emplace_back(width, y);
      a.emplace_back(width, y + 1);
    }
  }
}

void remove_Duplicates_And_Shrink(vector<Point>& v)
{
  auto last = unique(v.begin(), v.end());
  v.erase(last, v.end());
  v.shrink_to_fit();
}

int max_Kills_Horizontal(const vector<Point>& v, const int width)
{
  int maximum = 0;
  
  for (auto left_end = !(v.front() == Point(-1, -1)) ? v.begin() : v.begin() + 1, right_end = left_end + 1
    ; right_end != v.end()
    ; ++left_end, ++right_end)
  {
    int count = 0;
    const int xleft  = (left_end->x);
    int       xright = (right_end->x) + 1;

    if (left_end->y != right_end->y)
    {
      xright = width;
    }

    for (auto start = left_end; start != v.begin() && start->y > left_end->y - 2; --start)
    {
      if (start->x >= xleft && start->x <= xright && start->x != -1 && start->x != width)
      {
        ++count;
      }
    }
    for (auto end = right_end; end != v.end() && end->y < left_end->y + 2; ++end)
    {
      if (end->x >= xleft && end->x <= xright && end->x != -1 && end->x != width)
      {
        ++count;
      }
    }

    maximum = max(count, maximum);
  }

  return maximum;
}

int max_Kills_Vertical(const vector<Point>& v, const int height)
{
  int maximum = 0;

  for (auto top_end = !(v.front() == Point(-1, -1)) ? v.begin() : v.begin() + 1, bottom_end = top_end + 1
    ; bottom_end != v.end()
    ; ++top_end, ++bottom_end)
  {
    int count = 0;
    const int ytop = (top_end->y);
    int       ybottom = (bottom_end->y) + 1;

    if (top_end->x != bottom_end->x)
    {
      ybottom = height;
    }

    for (auto start = top_end; start != v.begin() && start->x > top_end->x - 2; --start)
    {
      if (start->y >= ytop && start->y <= ybottom && start->y != -1 && start->y != height)
      {
        ++count;
      }
    }
    for (auto end = bottom_end; end != v.end() && end->x < top_end->x + 2; ++end)
    {
      if (end->y >= ytop && end->y <= ybottom && end->y != -1 && end->y != height)
      {
        ++count;
      }
    }

    maximum = max(count, maximum);
  }

  return maximum;
}

int max_Kills_Diagonal(const vector<Point>& v, const int width, const int height)
{
  int maximum = 0;

  for (auto top_end = v.begin(), bottom_end = top_end + 1
    ; bottom_end != v.end()
    ; ++top_end, ++bottom_end)
  {
    int count = 0;
    const int ytop = (top_end->y);
    int       ybottom = (bottom_end->y) + 1;
    const int xleft = (top_end->x);
    int       xright = (bottom_end->x) + 1;

    if (diagonal_Index(*top_end) != diagonal_Index(*bottom_end))
    {
      ybottom = height - (top_end->x - top_end->y);
      xright = width - (top_end->y - top_end->x);
    }

    for (auto start = top_end; start != v.begin() && diagonal_Index(*start) > diagonal_Index(*top_end) - 3; --start)
    {
      if (start->y >= ytop && start->y <= ybottom
        && start->x >= xleft && start->x <= xright
        && start->y != -1 && start->y != height
        && start->x != -1 && start->x != width)
      {
        ++count;
      }
    }
    for (auto end = bottom_end; end != v.end() && diagonal_Index(*end) < diagonal_Index(*top_end) + 3; ++end)
    {
      if (end->y >= ytop && end->y <= ybottom
        && end->x >= xleft && end->x <= xright
        && end->y != -1 && end->y != height
        && end->x != -1 && end->x != width)
      {
        ++count;
      }
    }

    maximum = max(count, maximum);
  }

  return maximum;
}

int max_Kills_Anti_Diagonal(const vector<Point>& v, const int width, const int height)
{
  int maximum = 0;

  for (auto left_end = v.begin(), right_end = left_end + 1
    ; right_end != v.end()
    ; ++left_end, ++right_end)
  {
    int count = 0;
    int ytop = (right_end->y) - 1;
    const int ybottom = (left_end->y);
    const int xleft = (left_end->x);
    int       xright = (right_end->x) + 1;

    if (anti_Diagonal_Index(*left_end) != anti_Diagonal_Index(*right_end))
    {
      ytop = max(-1, left_end->y + left_end->x - width);
      xright = min(width, left_end->y + left_end->x + 1);
    }

    for (auto start = left_end; start != v.begin() && anti_Diagonal_Index(*start) > anti_Diagonal_Index(*left_end) - 3; --start)
    {
      if (start->y >= ytop && start->y <= ybottom
        && start->x >= xleft && start->x <= xright
        && start->y != -1 && start->y != height
        && start->x != -1 && start->x != width)
      {
        ++count;
      }
    }
    for (auto end = right_end; end != v.end() && anti_Diagonal_Index(*end) < anti_Diagonal_Index(*left_end) + 3; ++end)
    {
      if (end->y >= ytop && end->y <= ybottom
        && end->x >= xleft && end->x <= xright
        && end->y != -1 && end->y != height
        && end->x != -1 && end->x != width)
      {
        ++count;
      }
    }
    maximum = max(count, maximum);
  }

  return maximum;
}

int main(int argc, char* argv[])
{
  int height, width;
  cin >> height >> width;

  vector<Point> horizontal_points,
              vertical_points,
              diagonal_points,
              antidiagonal_points;

  populate_Point_Vectors(height, width,
    horizontal_points, vertical_points,
    diagonal_points, antidiagonal_points);

  sort(horizontal_points.begin(), horizontal_points.end(), horizontal_Compare);
  sort(vertical_points.begin(), vertical_points.end(), vertical_Compare);
  sort(diagonal_points.begin(), diagonal_points.end(), diagonal_Compare);
  sort(antidiagonal_points.begin(), antidiagonal_points.end(), anti_Diagonal_Compare);

  remove_Duplicates_And_Shrink(horizontal_points);
  remove_Duplicates_And_Shrink(vertical_points);
  remove_Duplicates_And_Shrink(diagonal_points);
  remove_Duplicates_And_Shrink(antidiagonal_points);

  cout << "Horizontal Maximum: " << max_Kills_Horizontal(horizontal_points, width) << endl;
  cout << "Vertical Maximum: " << max_Kills_Vertical(vertical_points, height) << endl;
  cout << "Diagonal Maximum: " << max_Kills_Diagonal(diagonal_points, width, height) << endl;
  cout << "Anti-Diagonal Maximum: " << max_Kills_Anti_Diagonal(antidiagonal_points, width, height) << endl;

  return 0;
}