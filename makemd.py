import sys

def main():
    with open(sys.argv[1], 'w') as f:
        f.write('a quick brown fox jumped over the lazy dog\n')

if __name__ == "__main__":
    main()
