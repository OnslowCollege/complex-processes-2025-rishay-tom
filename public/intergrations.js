const basePath = document.querySelector('meta[name="site-url"]').content.replace(/\/$/, '');
// prefix all /api/* calls with basePath, e.g basePath + "/api/*"

// data Intergration = Intergration
//   { name :: String
//   , status :: String
//   , type_ :: String
//   } deriving (Show, Generic, Eq)

// for /api/createintergration (enabling) do this

//IGNORE UNDER ME
//~~for /api/deleteintergration (disabling), just send the property 'intergration' with the name of the intergration to delete
// e.g minecraft~~

// /api/deleteintergration (disabling), should just send a string with the name of the intergration in the data portion of the request


async function toggleIntergration(button){
    // Since minecraft is the primary focus here, we will just hardcode the minecraft intergration
    // in other cases, we would have used button to get the name of the intergration and pass it in via apicall
    // I keep it here incase there is the intention to expand the amount of servers

//fetch status of minecraft intergrating and reverse elabled to disabled

  

    const integration = {
        // TODO: move kind outside of within element
        element: {
            data: {
                name: "minecraft",
                status: "enabled",
                type: "game"
            },
            kind: "Intergration"
        },
    };

    fetch(`${basePath}/api/createintergration`, {
        method: "POST",
        headers: {
            "Content-Type": "application/json"
        },
        body: JSON.stringify(integration)
    })
    .then(response => {
        if (!response.ok) {
            throw new Error("Failed to enable integration.");
        }
        return response.json();
    })
    .then(data => {
        toggleIntergration()
        console.log("Integration enabled:", data);
        // optionally update UI here using `button`
    })
    .catch(error => {
        console.error("Error enabling integration:", error);
    });
}
    

async function toggleIntergrationOnHomePage(button){
    // same reason I have the button here as I mentioned in enable intergration
    let isEnabled = ""

    
    const response = await fetch(`${basePath}/api/integrations`);
    console.log("Raw response:", response);

    const data = await response.json();
    console.log("Fetched JSON:", data);

    // Handle both array and object responses
    //const integrations = Array.isArray(data) ? data : data.intergrations || [];
    const integrations = data.integrations || [];
    console.log(integrations)

    integrations.forEach(integration => {
        console.log(integration)
        console.log("AHHHH1")
        //const element = document.querySelector(`[data-integration="${integration.name}"]`);
        if (integration.name == "minecraft") {
            if (integration.status == "enabled") {
                isEnabled = "disabled"
                console.log("Is Enabled: Disabled")
                
            }
            if (integration.status == "disabled") {
                isEnabled = "enabled"
                console.log("Is Enabled: Enabled")
            }
        } 
    });
    console.log(isEnabled)


    const integration = {
        element: {
            data: {
                name: "minecraft",
                status: isEnabled,
                type: "game"
            },
            kind: "Intergration"
        }
    };

    fetch(`${basePath}/api/updateintergration`, {
        method: "POST",
        headers: {
            "Content-Type": "application/json"
        },
        body: JSON.stringify(integration)
    })
    .then(response => {
        if (!response.ok) {
            throw new Error("Failed to enable integration on home page.");
        }
        return response.json();
    })
    .then(data => {
        fetchIntergrationColors()
        console.log("Integration enabled on home page:", data);
        // optionally update UI
    })
    .catch(error => {
        console.error("Error enabling integration on home page:", error);
    });
}

async function fetchIntergrationColors() {
    const basePath = document.querySelector('meta[name="site-url"]').content.replace(/\/$/, '');

    try {
        const response = await fetch(`${basePath}/api/integrations`);
        console.log("Raw response:", response);

        const data = await response.json();
        console.log("Fetched JSON:", data);

        // Handle both array and object responses
        //const integrations = Array.isArray(data) ? data : data.intergrations || [];
        const all_intergrations = document.querySelectorAll('[data-intergration]');
        all_intergrations.forEach(element_integration => {
            element_integration.backgroundColor = "red"
        })

        const integrations = data.integrations || [];

        //enable-intergration-button
        integrations.forEach(integration => {
            console.log("AHHHH")
            const found_intergration = document.querySelector(`[data-integration="${integration.name}"]`);
            if (found_intergration) {
                const inner_intergration = found_intergration.querySelector('#top-intergration-buttons');
                let inner_button = inner_intergration.querySelector(`#enable-intergration-button`);
                inner_button.style.backgroundColor = "lime"
                let inner_home_button = inner_intergration.querySelector(`#enable-intergration-on-home-button`);
                if (integration.status == "enabled"){
                    inner_home_button.style.backgroundColor = "lime"
                } else {
                    inner_home_button.style.backgroundColor = "red"
                }
                    // integration.status === "enabled" ? "green" : "grey";
            }
        });
        //enable-intergration-on-home-button

    } catch (error) {
        console.error("Failed to fetch integrations:", error);
    }
}

fetchIntergrationColors();
