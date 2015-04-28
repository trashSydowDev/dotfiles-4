#!/usr/bin/env rdmd
import std.algorithm : map, each;
import std.array : array;
import std.conv : to;
import std.file : SpanMode, dirEntries, getcwd, rename;
import std.getopt : getopt;
import std.path : extension, buildPath;
import std.regex : ctRegex, empty, matchFirst;
import std.stdio : writefln, writeln;

void main(string[] args) {
  string directory;

  // Parse command-line arguments
  getopt(
    args,
    "directory|d", &directory
  );

  if(directory == null) {
    directory = getcwd();
    writeln("Defaulting target the current working directory.");
  }

  // List the target directory
  auto youtubeDlR = ctRegex!(`^(.+)-[^\s]{11}\.[^.]+$`);
  auto results = dirEntries(directory, SpanMode.breadth).array();

  foreach(r; results) {
    auto rs = r.to!string;
    auto m = matchFirst(rs, youtubeDlR);
    if(m.empty) continue;

    auto nrs = m[1] ~ extension(rs);
    writefln("Moving %s to %s", rs, nrs);
    rename(buildPath(directory, rs), buildPath(directory, nrs));
  }
}
