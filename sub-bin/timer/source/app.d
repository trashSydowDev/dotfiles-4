import std.datetime : Clock, SysTime;
import std.process : environment;
import std.file : exists, isFile, mkdirRecurse, remove;
import std.path : buildPath, dirName;
import std.stdio : File, stderr, writeln, writefln;

int main(string[] args) {
  string homeDir;
  try {
    homeDir = environment["HOME"];
  } catch(Exception err) {
    stderr.writeln("Failed to find the $HOME directory");
    return 1;
  }

  auto targetPath = buildPath(homeDir, ".timer", "current");
  auto currentTime = Clock.currTime();

  mkdirRecurse(dirName(targetPath));

  if(args.length > 1) {
    auto command = args[1];
    switch(command) {
      case "start":
        return runStart(targetPath, currentTime);
      case "stop":
        return runStop(targetPath, currentTime);
      case "help":
        printHelp();
        return 0;
      default:
        printHelp();
        return 1;
    }
  }

  return runStart(targetPath, currentTime);
}

int runStop(string targetPath, SysTime currentTime) {
  if(!exists(targetPath) || !isFile(targetPath)) {
    stderr.writeln("Failed to find running timer");
    return 1;
  }

  SysTime timestamp = SysTime.fromSimpleString(File(targetPath, "r").readln);

  writeln(currentTime - timestamp,);
  remove(targetPath);

  return 0;
}

int runStart(string targetPath, SysTime currentTime) {
  File(targetPath, "w").write(currentTime);

  writefln(
    "Start %02d:%02d:%02d written to %s",
    currentTime.hour(),
    currentTime.minute(),
    currentTime.second(),
    targetPath
  );

  return 0;
}

void printHelp() {
  writeln("Usage: timer [start|stop]");
}
