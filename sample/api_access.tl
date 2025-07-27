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
    user = @(try data[user_id] except KeyError())
        .map_err(e => KeyError(f"User {user_id} not found: {e}"))

    profile = try user.profile

    profile matches not Ok() then:
        print(f"Profile not found.")
    else:
        print(f"Name: {try profile.name ?? "N/A"}")

        contact = try profile.contact

        contact matches not Ok() then:
            print(f"Contact not found.")
        else:
            print(f"Email: {try contact.email ?? "N/A"}")
            print(f"Phone: {try contact.phone ?? "N/A"}")

    orders = try user.orders ?? []
    n_orders = @(try len(orders))
        .map_err(e => ValueError("Orders is malformed."))

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
        get_user_details(API_RESPONSE, id) match:
            Err() as e => print(f"Error fetching details: {repr(e)}")
            None => print("Unexpected return status!")
            default print("Details fetched successfully.")
        print("-" * 20))
    .list()