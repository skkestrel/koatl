API_RESPONSE = {
    user_a: {
        profile: {
            name: "Alice"
            contact: {email: "alice@example.com", phone: None}
        }
        orders: [{id: 1, total: 100}, {id: 2, total: 150}]
    }
    user_b: {
        # contact is missing
        profile: {name: "Bob"}
        orders: []
    }
    user_c: {
        profile: None
        # Invalid orders data
        orders: 1
    }
}

get_user_details = (data, user_id) =>
    let user = @(check data[user_id] except KeyError())
        .result.map_err(e => KeyError(f"User {user_id} not found: {e}"))

    let profile = check user.profile

    profile matches Ok(profile) then:
        print(f"Name: {check profile.name ?? "N/A"}")

        let contact = check profile.contact

        contact matches Ok(contact) then:
            print(f"Email: {check contact.email ?? "N/A"}")
            print(f"Phone: {check contact.phone ?? "N/A"}")
        else:
            print(f"Contact not found.")
    else:
        print(f"Profile not found.")

    let orders = check user.orders ?? []
    let n_orders = @(check len(orders))
        .result.map_err(e => ValueError("Orders is malformed."))

    print(
        n_orders > 0 then:
            f"Number of orders: {n_orders}"
        else:
            "No orders found."
    )

    ()

["user_a", "user_b", "user_c", "user_d"]
    .map(id =>
        print(f"Searching for id: {id}")
        get_user_details(API_RESPONSE, id).result match:
            Err(e) => print(f"Error fetching details: {repr(e)}")
            Ok() => print("Details fetched successfully.")
        print("-" * 20))
    .list()