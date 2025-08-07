import requests
import datetime.(date, datetime)
import time
import sys

countdown = target_dt =>
    print(f"Countdown to sunset at {format(target_dt.strftime("%-I:%M:%s %p"))}")
    for i in ..:
        let remaining = target_dt - datetime.now(target_dt.tzinfo)
        if remaining.total_seconds() <= 0:
            print("The sun has set!")
            break

        print("Time remaining: {}".format(str(remaining).split(".")[0]))
        time.sleep(1)

if __name__ == "__main__":
    let lat = 49.1666
    let lng = -123.1336
    let today_str = date.today().isoformat()
    let api_url = f"https://api.sunrise-sunset.org/json?lat={lat}&lng={lng}&date={today_str}&formatted=0"

    print(f"Getting sunset time for {today_str}...")

    let response = requests.get(api_url)
    response.raise_for_status()
    let data = response.json()

    if data["status"] == "OK":
        let sunset_utc = datetime.fromisoformat(data["results"]["sunset"])
        let sunset_local = sunset_utc.astimezone(tz=None)

        print("Success! Sunset is at {}.".format(
            sunset_local.strftime("%-I:%M %p local time")
        ))
        countdown(sunset_local)
    else:
        print("API Error")

