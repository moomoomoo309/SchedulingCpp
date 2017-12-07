#include <iostream>
#include <array>
#include <utility>
#include <vector>
#include <map>
#include <cmath>
#include <functional>
#include <sstream>
#include <gsl-lite.h>

#define ulong unsigned long

enum EventType {
    start,
    deadline,
    process,
    noProcess
};

struct event {
    EventType type;
    double time;
    int process;
};

union fmtArg {
    int i;
    double d;
};

struct logEntry {
    std::string formatString;
    std::vector<fmtArg> parameters;
};

struct Process {
    double start, deadline, period, execution;

    bool operator<(const Process &p) const {
        return start < p.start;
    }
};

std::map<Process, bool> scheduled = std::map<Process, bool>();

struct Schedule {
    std::vector<event> events;
    std::vector<logEntry> log;
    bool feasible = false;
};

struct TableCell {
    std::string content;
    int colSpanIndex = -1;
};

typedef int (* schedulingAlgorithm)(std::vector<Process>, double);

double gcf(double a, double b) {
    a = std::abs(a);
    b = std::abs(b);
    if (b == 0)
        return a;
    return gcf(b, fmod(a, b));
}

double LCM(double a, double b) {
    return a * b / gcf(a, b);
}

template<typename array>
double lcm(array arr, ulong len) {
    double last = NAN;
    for (int i = 0; i < len; i++) {
        last = last == NAN ? arr[i]: LCM(last, arr[i]);
    }
}

int earliestDeadlineFirst(std::vector<Process> processes, double time) {
    double remainingTime = INFINITY;
    int index = -1;
    for (int i = 0; i < processes.size(); i++) {
        Process process = processes[i];
        if (scheduled[process] and time - process.start >= 0) {
            double remTime = fmod((time - process.start), process.deadline);
            if (remTime < remainingTime) {
                remainingTime = remTime;
                index = i;
            }
        }
    }
    return index;
}

int deadlineMonotonic(std::vector<Process> processes, double) {
    double minDeadline = INFINITY;
    int index = -1;
    for (int i = 0; i < processes.size(); i++) {
        Process process = processes[i];
        if (scheduled[process] and process.deadline < minDeadline) {
            minDeadline = process.deadline;
            index = i;
        }
    }
    return index;
}

const std::map<char, std::string> thinBorders = {
        {3,  "─"},
        {5,  "┘"},
        {6,  "└"},
        {7,  "┴"},
        {9,  "┐"},
        {10, "┌"},
        {11, "┬"},
        {12, "│"},
        {13, "┤"},
        {14, "├"},
        {15, "┼"}
};

const std::map<char, std::string> thickBorders = {
        {3,  "━"},
        {5,  "┛"},
        {6,  "┗"},
        {7,  "┻"},
        {9,  "┓"},
        {10, "┏"},
        {11, "┳"},
        {12, "┃"},
        {13, "┫"},
        {14, "┣"},
        {15, "╋"}
};

class MalformedTable : std::exception {
public:
    explicit MalformedTable(std::string message) : msg(std::move(message)) {}

    const char* what() const throw() override {
        return msg.c_str();
    }

private:
    std::string msg = "Malformed Table!";
};

std::string getBorders(bool left, bool right, bool up, bool down, bool thick = true) {
    std::map<char, std::string> borders = thick ? thickBorders: thinBorders;
    return borders[(left ? 1: 0) + (right ? 2: 0) + (up ? 4: 0) + (down ? 8: 0)];
}

enum justify {
    LEFT,
    RIGHT,
    CENTER_LEFT,
    CENTER_RIGHT
};

std::string _getPad(const std::string &str, ulong len, const std::string &padChar = " ") {
    std::ostringstream out;
    ulong strLen = str.length();
    ulong numChars = len - strLen;
    numChars = numChars >= 0 ? numChars: 0;
    for (int i = 0; i < numChars / padChar.length(); i++)
        out << padChar;
    out.width(len);
    return out.str();
}

std::string pad(const std::string &str, ulong len, const std::string &padChar = " ", justify align = CENTER_LEFT) {
    switch (align) {
        case RIGHT:
            return _getPad(str, len, padChar) + str;
        case LEFT:
            return str + _getPad(str, len, padChar);
        case CENTER_LEFT:
        case CENTER_RIGHT:
            std::string padding = _getPad(str, len, padChar);
            return padding.substr(0, padding.length() / 2 + (align == CENTER_LEFT)) + str +
                   padding.substr(padding.length() / 2 + (align == CENTER_RIGHT));
    }
}

std::string formatTable(TableCell* tbl, const ulong &tblLen, ulong* columnLengths, const ulong &cols) {
    ulong rows = tblLen / cols;
    std::vector<ulong> newlineIndices = {0};
    std::vector<ulong> separatorIndices;
    std::vector<std::ostringstream> separatorRows;
    std::vector<std::ostringstream> contentRows;
    gsl::span<ulong> window;
    for (ulong i = rows * cols - 1; i >= 0; i--) {
        if (tbl[i].colSpanIndex % cols > i % cols)
            throw MalformedTable("Column spans cannot cross rows!");
        if (tbl[i].colSpanIndex == -1)
            separatorIndices.push_back(i);
    }

    std::string sep = getBorders(false, false, true, true);
    ulong contIndex = 0, sepIndex = 0;
    ulong lastSep = separatorIndices.size() - 1;
    for (ulong i = 0; i < rows * cols; i++) {
        if (i % cols == cols - 1) {
            contentRows[contIndex] << sep;
            contentRows[contIndex++] << std::endl;
            separatorIndices.push_back(i);
            newlineIndices.push_back(separatorIndices.size());
        }
        if (separatorIndices[sepIndex - i]) {
            contentRows[contIndex] << sep;
            separatorIndices.push_back(i);
        }
        contentRows[contIndex] << pad(tbl[i].content, columnLengths[i % cols]);
    }

    for (ulong i = 0; i < separatorIndices.size(); i += 2) {
        window = gsl::make_span(&separatorIndices[newlineIndices[i]], &separatorIndices[newlineIndices[i + 1]]);

    }

    return contentRows[0].str();
}

std::string formatSchedule(Schedule schedule, const std::vector<Process> &processes, double stopTime) {
    double timeStep = 1;
    {
        double lastTime = -1;
        for (auto event: schedule.events) {
            if (lastTime != -1) {
                double timeDelta = event.time - lastTime;
                if (timeDelta > 0)
                    timeStep = timeStep >= timeDelta ? timeStep: timeDelta;
            }
        }
    }
    ulong columnLengths[processes.size() + 2];
    auto tblLen = (ulong) ((processes.size() + 2) * ceil(stopTime / timeStep));
    TableCell tbl[tblLen];
    for (auto event: schedule.events) {
        auto row = (int) round(event.time / timeStep);
        ulong index = event.process + row * (processes.size() + 2);
        switch (event.type) {
            case deadline:
                tbl[index].content = ")" + tbl[index].content;
                break;
            case start:
                tbl[index].content = tbl[index].content + "(";
                break;
            case process:
                tbl[event.process].content = "J" + event.process;
                break;
            case noProcess:
                tbl[event.process].content = "X";
                break;
        }
    }
    for (int i = 0; i < tblLen; i++) {
        if (i < processes.size() + 2 and tbl[i].content.length() == 0)
            tbl[i].colSpanIndex = i > 0 and tbl[i - 1].colSpanIndex != -1 ? tbl[i - 1].colSpanIndex: i - 1;
        columnLengths[i] = columnLengths[i] > tbl[i].content.length() ? columnLengths[i]: tbl[i].content.length();
    }

    return formatTable(tbl, tblLen, columnLengths, processes.size() + 2);
}

Schedule
schedule(const std::vector<Process> &processes, schedulingAlgorithm* algorithm, double startTime, double &stopTime) {
    double time = startTime;
    double lastScheduleTime[processes.size()];
    struct Schedule schedule = {};
    for (int i = 0; i < processes.size(); i++)
        lastScheduleTime[i] = -1;
    while (stopTime >= time) {
        Process scheduledProcess{};
        //Start processes, get deadlines of processes
        for (int i = 0; i < processes.size(); i++) {
            Process process = processes[i];
            if (lastScheduleTime[i] != -1 and lastScheduleTime[i] + process.deadline == time) {
                if (scheduled[process])
                    schedule.feasible = false;
                schedule.log.push_back({"[%.1f] Process %d queued.", {(fmtArg) {.d=time}, (fmtArg) {.i=i}}});
                schedule.events.push_back((event) {deadline, time, i});
            }
            if (time - process.start >= 0 and fmod((time - process.start), process.period) <= 1e-8) {
                schedule.log.push_back({"[%s] Process %d queued.", {(fmtArg) {.d=time}, (fmtArg) {.i=i}}});
                scheduled[process] = true;
                schedule.events.push_back((event) {start, time, i});
                lastScheduleTime[i] = time;
            }
        }

        //Schedule processes
        int index = (*algorithm)(processes, time);

        if (index != -1) {
            scheduledProcess = processes[index];
            schedule.events.push_back((event) {process, time, index});
            scheduled[scheduledProcess] = false;
            time += processes[index].execution;
        } else {
            schedule.events.push_back((event) {noProcess, time});
            double nextTime = -1;
            for (auto process : processes) {
                double _nextTime = fmod(time - process.start, process.deadline);
                nextTime = (_nextTime > 0 and (nextTime == -1 or nextTime > _nextTime)) ? _nextTime: nextTime;
            }
            time += nextTime;
        }
    }
    return schedule;
}

std::array<bool, 2> feasibilityTests(std::vector<Process> processes) {
    double test1Sum = 0, test2Sum = 0;
    std::ostringstream sum1, sum2;
    for (int i = 0; i < processes.size(); i++) {
        Process process = processes[i];
        bool last = i == processes.size() - 1;
        double test1Term = process.execution / process.period;
        sum1 << test1Term << (last ? " = ": " + ");
        double test2Term = process.execution / process.deadline;
        sum2 << test2Term << (last ? " = ": " + ");
        test1Sum += test1Term;
        test2Sum += test2Term;
    }
    sum1 << test1Sum;
    sum2 << test2Sum;
    bool feasible = test1Sum > 1, infeasible = test2Sum <= 1;
    std::cout << sum1.str() << (feasible ? " > 1": " <= 1") << std::endl;
    std::cout << (feasible ? "The given processes are feasible.": "The first test was inconclusive.") << std::endl;
    if (not feasible) {
        std::cout << std::endl << sum2.str() << (infeasible ? " <= 1": " > 1") << std::endl;
        std::cout << (infeasible ? "The given processes are infeasible.": "The second test was inconclusive.")
                  << std::endl;
    }
    return {feasible, infeasible};
};


double getStopTime(std::vector<Process> processes) {
    double maxS = -1;
    double pList[processes.size()];
    for (int i = 0; i < processes.size(); i++) {
        Process process = processes[i];
        maxS = process.start > maxS ? process.start: maxS;
        pList[i] = process.period;
    }
    double L = lcm(pList, processes.size());
    return maxS + 2 * L;
};

std::map<schedulingAlgorithm, std::string> names;

std::vector<Process> processes;
schedulingAlgorithm algorithm = deadlineMonotonic;

int main(int argc, char** argv) {
    names = {
            {earliestDeadlineFirst, "Earliest Deadline First"},
            {deadlineMonotonic,     "Deadline Monotonic"}
    };
    processes = {
            (Process) {.start = 0, .deadline = 3, .period = 4, .execution = .5},
            (Process) {.start = 2, .deadline = 3, .period = 4, .execution = 1},
            (Process) {.start = 2, .deadline = 2, .period = 4, .execution = .5},
            (Process) {.start = 1, .deadline = 3, .period = 3, .execution = 1}
    };
    std::cout << "Performing feasibility tests..." << std::endl;
    std::array<bool, 2> feasibility = feasibilityTests(processes);
    bool feasible = feasibility[0], infeasible = feasibility[1];
    if (not feasible and not infeasible) {
        std::cout << std::endl << "Creating schedule..." << std::endl << std::endl;
        double stopTime = getStopTime(processes);
        Schedule _schedule = schedule(processes, &algorithm, 0, stopTime);
        std::cout << (_schedule.feasible ? "The schedule is feasible.": "The schedule is infeasible.") << std::endl;
        std::cout << names[algorithm] << " scheduling algorithm being used." << std::endl;
        std::cout << std::endl << "Schedule:" << std::endl;
        std::cout << formatSchedule(_schedule, processes, stopTime) << std::endl;
        std::cout << std::endl << "Log:" << std::endl;
        for (const auto &entry: _schedule.log) {
            printf(entry.formatString.c_str(), entry.parameters[0], entry.parameters[1]);
        }
    }
    return 0;
}
