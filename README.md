Learning By Crawling - A Haskell Project
===================
- - - - 

# Getting Started # 
In order to get the program running some installations and other preparations are needed. Such as installing all the external libraries mentioned in the External libraries subsection and having a valid League of Legends developer key from Riot Games. Having a stable internet connection is also a requirement to run the program since we are getting data from Riot Games application programming interface (API) through the web.

## External libraries ##
All external libraries used in the program are described below. The program requires the user to have these libraries installed on their computer. All of the libraries are installed and downloaded through Haskell’s own package manager Cabal. Which can easily be used through a command-line interpreter (CLI) of choice. For example windows Command Prompt, MacOS or Linux terminal. The command to install are as follows: cabal install <Name of library>. Simply type the command into the CLI to install a library.

### Aeson - Data.Aeson v.1.2.4.0 ###
The Aeson library is used to get types and functions for parsing JSON data. Using this library makes it convenient to work with the JSON data our program retrieves from Riot Games API. For further information about the library see their documentation.

### Wreq - Network.Wreq v.0.5.2.0 ###
The Wreq library is used for client-side HTTP requests. The program uses this library to make requests towards the Riot Games API. Using this package the response body, headers and status are easily retrieved from a url (the API is accessed through a url). The response body is in our case the JSON data from the url. Response headers contain the request limit towards their API. Response status contains the status code for the request, the status code tells us if an error occurred or if the request was successful. For further information about the library see their documentation.

### Developer Key - Riot Games ###
To run our program a developer key from Riot Games is required. To make requests towards their API the key is required in the url, see the example below. https://euw1.api.riotgames.com/lol/summoner/v3/summoners/by-name/sprittiiy?api_key=RGAPI-9322e3b3-ebea-47ff-a298-e4cee9379411

In the program the key is used to get access to Riot Games API and to construct valid urls to do requests against their API.
The key is acquired through Riot Games developer website or as they call it, Riot Developer Portal. To get a developer key you must register on their website. After registering and logging in, head to dashboard and then under the section Development API Key you can find your personal key.

There are two types of keys: development keys and production keys. By standard a developer account is granted a development key and a production key is obtained by registering a permanent project to Riot Games. These keys both have rate limits which control the amount of requests a user can do towards their API. See rate limits below.

**Development key** - by design, very limited and expires after 24 hours.
20 requests every 1 second
100 requests every 2 minutes

**Production key** - much larger rate limit than a development key.
3 000 requests every 10 seconds
180 000 requests every 10 minutes

See Riot Developer Portal for further documentation about their keys and their API.

## Running the program ##
To run the program some descriptions order. How the modules actually work will be described in further detail in our documentation under the sections *The Crawler* and *Machine Learning*.

Initiating the crawler requires you to input an account ID that belongs to a player on Europe West with at least a single ranked solo 5v5 game played. A number of account ID’s will be supplied for testing purposes in file DataTypeExamples.hs with the format: accid1, accid2 etc. It is important to note that these account ID’s can be changed and there is no guarantee that they can be used indefinitely. After the user has chosen a legitimate account ID the crawler is engaged by calling: initiateGather “START” accID - on that account ID. Resuming the gather, similarly, by calling: initiateGather “whateverStringThatIsntSTART” accID2, where accID2 is another legitimate account ID. If using the supplied account id’s calling, initiateGather “START” accid1 will suffice.

As a side note, it is suboptimal to rerun the crawler on the same account ID multiple times, even though it is possible. This will cause the machine learning algorithm to learn on the same data multiple times and hence make misconstrued adjustments. Additionally, if the crawler is interrupted manually or because of unknown errors the storePlayers.json file needs to be cleared manually before resuming the crawler.

Initiating the machine learning part of the program is accomplished by simply calling function: startML, which will read the data from GameStorage.json where the crawler stored all it’s fetched data. Before calling startML however, a final “]” bracket has to be added to GameStorage.json, for the program to be able to read the file.
