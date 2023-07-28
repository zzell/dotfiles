if [ "$#" -ne 1 ]; then
  echo "Illegal number of params, message expeted."
  exit 1
fi

git commit -m "[$(git branch --show-current)] $1"
