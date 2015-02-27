import sys
import glob
import json
import os

class PandocMarkdownWriter(object):
    def __init__(self, f):
        self.f = f

    def heading(self, text, level):
        assert level >= 1 and level <= 6
        assert "\n" not in text
        self.f.write("#" * level + " " + text + "\n")

    def h1(self, text):
        self.heading(text, 1)

    def h2(self, text):
        self.heading(text, 2)

    def h3(self, text):
        self.heading(text, 3)

    def h4(self, text):
        self.heading(text, 4)

    def h5(self, text):
        self.heading(text, 5)

    def h6(self, text):
        self.heading(text, 6)

    def code(self, programtext, lang, number_lines=True):
        nlines = ".numberLines" if number_lines else ""
        self.f.write("~" * 4 + " {#mycode .%s %s}\n" % (lang, nlines))
        self.f.write(programtext)
        self.f.write("\n" + "~" * 20 + "\n")

def read_contents(filepath):
    with open(filepath) as f:
        return f.read()

def add_solution(writer, chapter, problem, filepath):
    writer.h3("Problem %d.%d" % (chapter, problem))
    basename, ext = os.path.splitext(filepath)
    ext = ext.lower()
    if ext == ".scm":
        writer.code(read_contents(filepath), "scheme", False)
    else:
        print "Warning: Dont know how to handle file '%s' of type: '%s'" % (
                filepath, ext)

def generate(toc, writer):
    for idx, chapterinfo in enumerate(toc):
        chapter = idx + 1
        name, folder, problems = chapterinfo['name'], chapterinfo['folder'], \
                chapterinfo['problems']
        writer.h1("Chapter %d" % (chapter))
        writer.h2(name)
        for problem in problems:
            files = glob.glob(os.path.join(folder, "E%s.%s.*" % (chapter, problem)))
            if len(files) == 0:
                print "Warning: No solution found for exercise %d.%d" % (chapter, problem)
                continue
            elif len(files) != 1:
                print "Warning: Found %s files for problem %d. Expecting 1" % (
                        len(files), problem, repr(files))
            print "Processing:", files[0]
            add_solution(writer, chapter, problem, files[0])

def main():
    if len(sys.argv) != 3:
        print "Usage: %s <toc file> <output file>" % (sys.argv[0])
        return

    with open(sys.argv[1]) as tocfile, open(sys.argv[2], 'w') as f:
        toc = json.load(tocfile)
        writer = PandocMarkdownWriter(f)
        generate(toc, writer)

if __name__ == "__main__":
    main()
