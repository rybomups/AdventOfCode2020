const baseURL = "https://adventofcode.com/2020/day/%s/input";

async function readInput(day) {
    const url = baseURL.replace("%s", String(day));

    try {
        // Use session token from environment variable
        const session = process.env.AOC_SESSION;

        const response = await fetch(url, {
            method: "GET",
            headers: {
                Cookie: `session=${session}`
            }
        });

        if (!response.ok) {
            console.error(`Unexpected HTTP status: ${response.status}`);
            return;
        }

        const body = await response.text();

        process.stdout.write(body);

    } catch (error) {
        console.error(`Error making HTTP request: ${error.message}`);
    }
}

async function main() {
    if (process.argv[2] === undefined)
        console.error("Please provide the number of a day to fetch");
    else
        await readInput(process.argv[2]);
}

main();