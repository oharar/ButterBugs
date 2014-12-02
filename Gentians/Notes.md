# Notes

Combining the code is straightforward, the bigger problem is formatting the data: we need to make sure the variables are unique (which is done by using tags: .tag in the sub-models, wich are replaced in teh code by specific tags). There is also some messing around to make sure the loops are set to be teh correct length (rather than relying on the user to do this!).

This is a simple proof of concept, which works for the data and model provided, but might not work for other problems. The code can surely be improved a lot: 'it works' is perhaps the best description at the moment.

## What this shows

- getting tags on variables right will be important: at the moment we have 3 types
- having data in different sub-models makes things tricky, especially if they are indexed with other variables. It's possible that the present code solves this, but that is unlikely.
- some thought should be put into creating formal objects: this will clean up the code.
- there is a lot of messing around with strings. This was done using base R and can probably be improved considerably if there are better R packages, or if another language is used.
