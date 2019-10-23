inotifywait -q -m -e close_write Prop.hs |
while read -r filename event; do
  clear
  stack runhaskell Prop.hs
done
